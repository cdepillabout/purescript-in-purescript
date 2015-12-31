
module Language.PureScript.Crash where

import Prelude ((<>), ($), (<<<))

import Control.Monad.Eff.Exception (error, throwException)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

-- | Exit with an error message and a crash report link.
internalError :: forall a . String -> a
internalError errMsg =
    unsafePerformEff <<< throwException <<< error $
        "An internal error ocurred during compilation: " <> errMsg <>
        "\nPlease report this at https://github.com/purescript/purescript/issues"
