
module Language.PureScript.Types where

import Prelude

import Data.Array
import Data.Foldable
import Data.Maybe (Maybe(..))
import Data.Tuple

import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.Traversals
import Language.PureScript.SourcePos

-- | An identifier for the scope of a skolem variable
newtype SkolemScope = SkolemScope Int
-- deriving (Show, Read, Eq, Ord, Data, Typeable, A.ToJSON, A.FromJSON)

runSkolemScope :: SkolemScope -> Int
runSkolemScope (SkolemScope int) = int

-- |
-- The type of types
--
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

-- | A typeclass constraint
type Constraint = Tuple (Qualified ProperName) (Array Type)
-- $(A.deriveJSON A.defaultOptions ''Type)

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
