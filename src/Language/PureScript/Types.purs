
module Language.PureScript.Types where

import Prelude
    ( class Monad, class Applicative, class Functor, class Eq, class Show, bind
    , map, ($), (>>=), (<$>), (<*>), pure, (<<<), (||), (<>), show, (+), (/=)
    , eq
    )

import Control.Bind ((<=<))
import Data.Array (concatMap, (:), nub, filter, uncons)
import Data.Foldable (foldl, foldr, notElem, elem)
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), snd, fst, lookup)

import Language.PureScript.Names (ProperName, Qualified)
import Language.PureScript.Kinds (Kind)
import Language.PureScript.Traversals (sndM)
import Language.PureScript.SourcePos (SourceSpan)

-- | An identifier for the scope of a skolem variable
newtype SkolemScope = SkolemScope Int
-- deriving (Show, Read, Eq, Ord, Data, Typeable, A.ToJSON, A.FromJSON)
derive instance genericSkolemScope :: Generic SkolemScope
instance showSkolemScope :: Show SkolemScope where show = gShow

runSkolemScope :: SkolemScope -> Int
runSkolemScope (SkolemScope int) = int

-- | The type of types
data Type
  -- | A unification variable of type Type
  = TUnknown Int
  -- | A named type variable
  | TypeVar String
  -- | A type wildcard, as would appear in a partial type synonym
  | TypeWildcard
  -- | A type constructor
  | TypeConstructor (Qualified ProperName)
  -- | A type application
  | TypeApp Type Type
  -- | Forall quantifier
  | ForAll String Type (Maybe SkolemScope)
  -- | A type with a set of type class constraints
  | ConstrainedType (Array Constraint) Type
  -- | A skolem constant
  | Skolem String Int SkolemScope (Maybe SourceSpan)
  -- | An empty row
  | REmpty
  -- | A non-empty row
  | RCons String Type Type
  -- | A type with a kind annotation
  | KindedType Type Kind
  -- | A placeholder used in pretty printing
  | PrettyPrintFunction Type Type
  -- | A placeholder used in pretty printing
  | PrettyPrintObject Type
  -- | A placeholder used in pretty printing
  | PrettyPrintForAll (Array String) Type
-- deriving (Show, Read,Eq, Ord, Data, Typeable)
derive instance genericType :: Generic Type
instance showType :: Show Type where show = gShow
instance eqType :: Eq Type where eq = gEq

-- | A typeclass constraint
type Constraint = Tuple (Qualified ProperName) (Array Type)
-- $(A.deriveJSON A.defaultOptions ''Type)

-- | Return list of types under a constraint.
constraintTypes :: Constraint -> Array Type
constraintTypes = snd

mapConstraintTypes :: (Type -> Type) -> Constraint -> Constraint
mapConstraintTypes f (Tuple names typeArray) = Tuple names $ map f typeArray

-- | Convert a row to a list of pairs of labels and types
rowToList :: Type -> Tuple (Array (Tuple String Type)) Type
rowToList (RCons name ty row) =
    -- let (Tuple tys rest) = rowToList row
    -- in Tuple (Tuple name ty : tys) rest
    let tuple = rowToList row
        tys = fst tuple
        rest = snd tuple
    in Tuple (Tuple name ty : tys) rest
rowToList r = Tuple [] r

-- | Convert a list of labels and types to a row
rowFromList :: Tuple (Array (Tuple String Type)) Type -> Type
rowFromList (Tuple list r) =
    case uncons list of
        Nothing -> r
        Just { head: Tuple name t, tail: ts } ->
            RCons name t <<< rowFromList $ Tuple ts r

-- | Check whether a type is a monotype
isMonoType :: Type -> Boolean
isMonoType (ForAll _ _ _) = false
isMonoType _              = true

-- | Universally quantify a type
mkForAll :: Array String -> Type -> Type
mkForAll args ty = foldl (\t arg -> ForAll arg t Nothing) ty args

-- | Replace a type variable, taking into account variable shadowing
replaceTypeVars :: String -> Type -> Type -> Type
replaceTypeVars v r = replaceAllTypeVars [Tuple v r]

-- | Replace named type variables with types
replaceAllTypeVars :: Array (Tuple String Type) -> Type -> Type
replaceAllTypeVars = go []
  where
    go :: Array String -> Array (Tuple String Type) -> Type -> Type
    go _  m (TypeVar v) = fromMaybe (TypeVar v) (v `lookup` m)
    go bs m (TypeApp t1 t2) = TypeApp (go bs m t1) (go bs m t2)
    go bs m f@(ForAll v t sco) =
        if v `elem` keys
            then
                go bs (filter ((/= v) <<< fst) m) f
            else
                if v `elem` usedVars
                    then
                        let v' = genName v (keys <> bs <> usedVars)
                            t' = go bs [Tuple v (TypeVar v')] t
                        in ForAll v' (go (v' : bs) m t') sco
                    else
                        ForAll v (go (v : bs) m t) sco
      where
        keys :: Array String
        keys = map fst m

        usedVars :: Array String
        usedVars = concatMap (usedTypeVariables <<< snd) m

    go bs m (ConstrainedType cs t) = ConstrainedType (map (mapConstraintTypes (go bs m)) cs) (go bs m t)
    go bs m (RCons name' t r) = RCons name' (go bs m t) (go bs m r)
    go bs m (KindedType t k) = KindedType (go bs m t) k
    go _  _ ty = ty

    genName :: String -> Array String -> String
    genName orig inUse = try 0
      where
        try :: Int -> String
        try n = if (orig <> show n) `elem` inUse
                    then try $ n + 1
                    else orig <> show n

-- | Collect all type variables appearing in a type
usedTypeVariables :: Type -> Array String
usedTypeVariables = nub <<< everythingOnTypes (<>) go
  where
    go :: Type -> Array String
    go (TypeVar v) = [v]
    go _ = []

-- | Collect all free type variables appearing in a type
freeTypeVariables :: Type -> Array String
freeTypeVariables = nub <<< go []
  where
    go :: Array String -> Type -> Array String
    go bound (TypeVar v) = if v `notElem` bound then [v] else []
    go bound (TypeApp t1 t2) = go bound t1 <> go bound t2
    go bound (ForAll v t _) = go (v : bound) t
    go bound (ConstrainedType cs t) = concatMap (concatMap (go bound) <<< snd) cs <> go bound t
    go bound (RCons _ t r) = go bound t <> go bound r
    go bound (KindedType t _) = go bound t
    go _ _ = []

-- | Universally quantify over all type variables appearing free in a type
quantify :: Type -> Type
quantify ty = foldr (\arg t -> ForAll arg t Nothing) ty $ freeTypeVariables ty

-- | Move all universal quantifiers to the front of a type
--
-- TODO: This doesn't actually seem like it moves all the universal quantifiers
-- to the front of the type. For instance, in the following type, it doesn't
-- move the last quantifier to the front:
--
-- ```purescript
-- f :: Int -> (forall b . b)
-- ```
moveQuantifiersToFront :: Type -> Type
moveQuantifiersToFront = go [] []
  where
    go :: Array (Tuple String (Maybe SkolemScope)) -> Array Constraint -> Type -> Type
    go qs cs (ForAll q ty sco) = go (Tuple q sco : qs) cs ty
    go qs cs (ConstrainedType cs' ty) = go qs (cs <> cs') ty
    go [] [] ty = ty
    go [] cs ty = ConstrainedType cs ty
    go qs [] ty =
        foldl (\ty' (Tuple q sco) -> ForAll q ty' sco) ty qs
    go qs cs ty =
        foldl (\ty' (Tuple q sco) -> ForAll q ty' sco) (ConstrainedType cs ty) qs

-- | Check if a type contains wildcards
containsWildcards :: Type -> Boolean
containsWildcards = everythingOnTypes (||) go
  where
    go :: Type -> Boolean
    go TypeWildcard = true
    go _ = false

----------------
-- Traversals --
----------------

everywhereOnTypes :: (Type -> Type) -> Type -> Type
everywhereOnTypes f = go
  where
    go :: Type -> Type
    go (TypeApp t1 t2) = f (TypeApp (go t1) (go t2))
    go (ForAll arg ty sco) = f (ForAll arg (go ty) sco)
    go (ConstrainedType cs ty) = f (ConstrainedType (map (map (map go)) cs) (go ty))
    go (RCons name ty rest) = f (RCons name (go ty) (go rest))
    go (KindedType ty k) = f (KindedType (go ty) k)
    go (PrettyPrintFunction t1 t2) = f (PrettyPrintFunction (go t1) (go t2))
    go (PrettyPrintObject t) = f (PrettyPrintObject (go t))
    go (PrettyPrintForAll args t) = f (PrettyPrintForAll args (go t))
    go other = f other

everywhereOnTypesTopDown :: (Type -> Type) -> Type -> Type
everywhereOnTypesTopDown f = go <<< f
  where
    go :: Type -> Type
    go (TypeApp t1 t2) = TypeApp (go (f t1)) (go (f t2))
    go (ForAll arg ty sco) = ForAll arg (go (f ty)) sco
    go (ConstrainedType cs ty) = ConstrainedType (map (map (map (go <<< f))) cs) (go (f ty))
    go (RCons name ty rest) = RCons name (go (f ty)) (go (f rest))
    go (KindedType ty k) = KindedType (go (f ty)) k
    go (PrettyPrintFunction t1 t2) = PrettyPrintFunction (go (f t1)) (go (f t2))
    go (PrettyPrintObject t) = PrettyPrintObject (go (f t))
    go (PrettyPrintForAll args t) = PrettyPrintForAll args (go (f t))
    go other = f other

everywhereOnTypesM :: forall m . (Functor m, Applicative m, Monad m)
                   => (Type -> m Type) -> Type -> m Type
everywhereOnTypesM f = go
  where
    go :: Type -> m Type
    go (TypeApp t1 t2) = (TypeApp <$> go t1 <*> go t2) >>= f
    go (ForAll arg ty sco) = (ForAll arg <$> go ty <*> pure sco) >>= f
    go (ConstrainedType cs ty) = (ConstrainedType <$> traverse (sndM (traverse go)) cs <*> go ty) >>= f
    go (RCons name ty rest) = (RCons name <$> go ty <*> go rest) >>= f
    go (KindedType ty k) = (KindedType <$> go ty <*> pure k) >>= f
    go (PrettyPrintFunction t1 t2) = (PrettyPrintFunction <$> go t1 <*> go t2) >>= f
    go (PrettyPrintObject t) = (PrettyPrintObject <$> go t) >>= f
    go (PrettyPrintForAll args t) = (PrettyPrintForAll args <$> go t) >>= f
    go other = f other

everywhereOnTypesTopDownM :: forall m . (Functor m, Applicative m, Monad m)
                          => (Type -> m Type) -> Type -> m Type
everywhereOnTypesTopDownM f = go <=< f
  where
    go :: Type -> m Type
    go (TypeApp t1 t2) = TypeApp <$> (f t1 >>= go) <*> (f t2 >>= go)
    go (ForAll arg ty sco) = ForAll arg <$> (f ty >>= go) <*> pure sco
    go (ConstrainedType cs ty) = ConstrainedType <$> traverse (sndM (traverse (go <=< f))) cs <*> (f ty >>= go)
    go (RCons name ty rest) = RCons name <$> (f ty >>= go) <*> (f rest >>= go)
    go (KindedType ty k) = KindedType <$> (f ty >>= go) <*> pure k
    go (PrettyPrintFunction t1 t2) = PrettyPrintFunction <$> (f t1 >>= go) <*> (f t2 >>= go)
    go (PrettyPrintObject t) = PrettyPrintObject <$> (f t >>= go)
    go (PrettyPrintForAll args t) = PrettyPrintForAll args <$> (f t >>= go)
    go other = f other

-- | Take a function from `Type -> r`, a combining function `r -> r -> r`,
-- convert every type to `r`, and then combine them together.
everythingOnTypes :: forall r . (r -> r -> r) -> (Type -> r) -> Type -> r
everythingOnTypes combiner f = go
  where
    go :: Type -> r
    go t@(TypeApp t1 t2) = f t `combiner` go t1 `combiner` go t2
    go t@(ForAll _ ty _) = f t `combiner` go ty
    go t@(ConstrainedType cs ty) = foldl combiner (f t) (map go $ concatMap constraintTypes cs) `combiner` go ty
    go t@(RCons _ ty rest) = f t `combiner` go ty `combiner` go rest
    go t@(KindedType ty _) = f t `combiner` go ty
    go t@(PrettyPrintFunction t1 t2) = f t `combiner` go t1 `combiner` go t2
    go t@(PrettyPrintObject t1) = f t `combiner` go t1
    go t@(PrettyPrintForAll _ t1) = f t `combiner` go t1
    go other = f other

-- | This is similar to `everythingOnTypes`, with `s` acting like the `s` from a State monad.
everythingWithContextOnTypes :: forall s r . s -> r -> (r -> r -> r) -> (s -> Type -> Tuple s r) -> Type -> r
everythingWithContextOnTypes s0 r0 combiner f = go' s0
  where
    go' :: s -> Type -> r
    go' s t = case f s t of
                  Tuple s' r -> r `combiner` go s' t

    go :: s -> Type -> r
    go s (TypeApp t1 t2) = go' s t1 `combiner` go' s t2
    go s (ForAll _ ty _) = go' s ty
    go s (ConstrainedType cs ty) = foldl combiner r0 (map (go' s) $ concatMap constraintTypes cs) `combiner` go' s ty
    go s (RCons _ ty rest) = go' s ty `combiner` go' s rest
    go s (KindedType ty _) = go' s ty
    go s (PrettyPrintFunction t1 t2) = go' s t1 `combiner` go' s t2
    go s (PrettyPrintObject t1) = go' s t1
    go s (PrettyPrintForAll _ t1) = go' s t1
    go _ _ = r0
