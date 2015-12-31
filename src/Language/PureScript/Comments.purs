
module Language.PureScript.Comments where

data Comment
  = LineComment String
  | BlockComment String
-- deriving (Show, Read, Eq, Ord, D.Data, D.Typeable)
-- $(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''Comment)
