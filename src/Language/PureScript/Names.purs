
module Language.PureScript.Names where

import Prelude (class Show, bind, (++), (/=), (<<<), map, ($), (<$>), (<>), show)

import Control.Monad.Supply.Class (class MonadSupply, fresh)
import Data.Array (filter)
import Data.Foldable (intercalate)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (split)
import Data.Tuple (Tuple(Tuple))

-- | Names for value identifiers
data Ident
  -- | An alphanumeric identifier
  = Ident String
  -- | A symbolic name for an infix operator
  | Op String
  -- | A generated name for an identifier
  | GenIdent (Maybe String) Int
-- deriving (Show, Read, Eq, Ord, Data, Typeable)
derive instance genericIdent :: Generic Ident
instance showIdent :: Show Ident where show = showIdent'

runIdent :: Ident -> String
runIdent (Ident i) = i
runIdent (Op op) = op
runIdent (GenIdent Nothing n) = "$" <> show n
runIdent (GenIdent (Just name) n) = "$" <> name <> show n

showIdent' :: Ident -> String
showIdent' (Op op) = "(" <> op <> ")"
showIdent' i = runIdent i

freshIdent :: forall m . (MonadSupply m) => String -> m Ident
freshIdent name = GenIdent (Just name) <$> fresh

freshIdent' :: forall m . (MonadSupply m) => m Ident
freshIdent' = GenIdent Nothing <$> fresh

-- | Proper names, i.e. capitalized names for e.g. module names, type//data constructors.
newtype ProperName = ProperName String
-- deriving (Show, Read, Eq, Ord, Data, Typeable)
derive instance genericProperName :: Generic ProperName
instance showProperName :: Show ProperName where show = gShow


runProperName :: ProperName -> String
runProperName (ProperName string) = string

-- | Module names
newtype ModuleName = ModuleName (Array ProperName)
-- deriving (Show, Read, Eq, Ord, Data, Typeable)
derive instance genericModuleName :: Generic ModuleName
instance showModuleName :: Show ModuleName where show = gShow

runModuleName :: ModuleName -> String
runModuleName (ModuleName pns) = intercalate "." $ map runProperName pns

moduleNameFromString :: String -> ModuleName
moduleNameFromString =
    ModuleName <<< map ProperName <<< filter (/= "") <<< split "."

-- | A qualified name, i.e. a name with an optional module name
data Qualified a = Qualified (Maybe ModuleName) a
-- deriving (Show, Read, Eq, Ord, Data, Typeable, Functor)
derive instance genericQualified :: Generic a => Generic (Qualified a)
instance showQualified :: Show a => Show (Qualified a) where show = showQualified' show

showQualified' :: forall a . (a -> String) -> Qualified a -> String
showQualified' f (Qualified Nothing a) = f a
showQualified' f (Qualified (Just name) a) = runModuleName name ++ "." ++ f a

-- instance (a ~ ProperName) => A.ToJSON (Qualified a) where
--   toJSON = A.toJSON . showQualified runProperName

-- instance (a ~ ProperName) => A.FromJSON (Qualified a) where
--   parseJSON =
--     A.withText "Qualified ProperName" $ \str ->
--       return $ case reverse (splitOn "." (T.unpack str)) of
--         [name]      -> Qualified Nothing (ProperName name)
--         (name:rest) -> Qualified (Just (reconstructModuleName rest)) (ProperName name)
--         _           -> Qualified Nothing (ProperName "")
--     where
--     reconstructModuleName = moduleNameFromString . intercalate "." . reverse

-- | Provide a default module name, if a name is unqualified
qualify :: forall a . ModuleName -> Qualified a -> Tuple ModuleName a
qualify m (Qualified Nothing a) = Tuple m a
qualify _ (Qualified (Just m) a) = Tuple m a

-- | Makes a qualified value from a name and module name.
mkQualified :: forall a . a -> ModuleName -> Qualified a
mkQualified name mn = Qualified (Just mn) name

-- | Remove the module name from a qualified name
disqualify :: forall a . Qualified a -> a
disqualify (Qualified _ a) = a

-- | Checks whether a qualified value is actually qualified with a module reference
isUnqualified :: forall a . Qualified a -> Boolean
isUnqualified (Qualified Nothing _) = true
isUnqualified _ = false

-- $(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''Ident)
-- $(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ProperName)
-- $(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ModuleName)

