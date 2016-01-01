
module Language.PureScript.Options where

import Prelude (class Show)

import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(Nothing))

-- | The data type of compiler options
newtype Options = Options {
    -- | Disable tail-call elimination
    optionsNoTco :: Boolean
    -- | Disable inlining of calls to return and bind for the Eff monad
  , optionsNoMagicDo :: Boolean
    -- | When specified, checks the type of `main` in the module, and generate a call to run main
    -- after the module definitions.
  , optionsMain :: Maybe String
    -- | Skip all optimizations
  , optionsNoOptimizations :: Boolean
    -- | Verbose error message
  , optionsVerboseErrors :: Boolean
    -- | Remove the comments from the generated js
  , optionsNoComments :: Boolean
    -- | The path to prepend to require statements
  , optionsRequirePath :: Maybe String
  }
derive instance genericOptions :: Generic Options
instance showOptions :: Show Options where show = gShow

-- |
-- Default make options
defaultOptions :: Options
defaultOptions = Options { optionsNoTco: false
                         , optionsNoMagicDo: false
                         , optionsMain: Nothing
                         , optionsNoOptimizations: false
                         , optionsVerboseErrors: false
                         , optionsNoComments: false
                         , optionsRequirePath: Nothing
                         }
