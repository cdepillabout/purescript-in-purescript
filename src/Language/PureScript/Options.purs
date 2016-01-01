
module Language.PureScript.Options where

import Prelude (class Show)

import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(Nothing))

-- | The data type of compiler options
newtype Options = Options {
    -- | Disable tail-call elimination
    noTco :: Boolean
    -- | Disable inlining of calls to return and bind for the Eff monad
  , noMagicDo :: Boolean
    -- | When specified, checks the type of `main` in the module, and generate a call to run main
    -- after the module definitions.
  , main :: Maybe String
    -- | Skip all optimizations
  , noOptimizations :: Boolean
    -- | Verbose error message
  , verboseErrors :: Boolean
    -- | Remove the comments from the generated js
  , noComments :: Boolean
    -- | The path to prepend to require statements
  , requirePath :: Maybe String
  }
derive instance genericOptions :: Generic Options
instance showOptions :: Show Options where show = gShow

-- |
-- Default make options
defaultOptions :: Options
defaultOptions = Options { noTco: false
                         , noMagicDo: false
                         , main: Nothing
                         , noOptimizations: false
                         , verboseErrors: false
                         , noComments: false
                         , requirePath: Nothing
                         }
