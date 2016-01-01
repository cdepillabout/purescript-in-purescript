
module Language.PureScript.TypeClassDictionaries where

import Prelude (class Show)

import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

import Language.PureScript.Names (ProperName, Qualified, Ident)
import Language.PureScript.Types (Constraint, Type)

-- | Data representing a type class dictionary which is in scope
newtype TypeClassDictionaryInScope
  = TypeClassDictionaryInScope {
    -- | The identifier with which the dictionary can be accessed at runtime
      name :: Qualified Ident
    -- | How to obtain this instance via superclass relationships
    , path :: Array (Tuple (Qualified ProperName) Int)
    -- | The name of the type class to which this type class instance applies
    , className :: Qualified ProperName
    -- | The types to which this type class instance applies
    , instanceTypes :: Array Type
    -- | Type class dependencies which must be satisfied to construct this dictionary
    , dependencies :: Maybe (Array Constraint)
    }
-- deriving (Show, Read, Data, Typeable)
derive instance genericTypeClassDictionaryInScope :: Generic TypeClassDictionaryInScope
instance showTypeClassDictionaryInScope :: Show TypeClassDictionaryInScope where show = gShow

-- | A simplified representation of expressions which are used to represent
-- type class dictionaries at runtime, which can be compared for equality
data DictionaryValue
  -- | A dictionary which is brought into scope by a local constraint
  = LocalDictionaryValue (Qualified Ident)
  -- | A dictionary which is brought into scope by an instance declaration
  | GlobalDictionaryValue (Qualified Ident)
  -- | A dictionary which depends on other dictionaries
  | DependentDictionaryValue (Qualified Ident) (Array DictionaryValue)
  -- | A subclass dictionary
  | SubclassDictionaryValue DictionaryValue (Qualified ProperName) Int
--  deriving (Show, Read, Ord, Eq)
derive instance genericDictionaryValue :: Generic DictionaryValue
instance showDictionaryValue :: Show DictionaryValue where show = gShow
