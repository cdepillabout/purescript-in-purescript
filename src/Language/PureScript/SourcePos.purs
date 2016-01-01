
module Language.PureScript.SourcePos where

import Prelude (class Show, bind, (<>), show)

import Data.Generic (class Generic, gShow)

-- | Source position information
newtype SourcePos = SourcePos
  { -- | Line number
    line :: Int
    -- | Column number
  , column :: Int
  }
-- deriving (Show, Read, Eq, Ord, D.Data, D.Typeable)
derive instance genericSourcePos :: Generic SourcePos
instance showSourcePos :: Show SourcePos where show = gShow

displaySourcePos :: SourcePos -> String
displaySourcePos (SourcePos sourcePos) =
  "line " <> show sourcePos.line <> ", column " <> show sourcePos.column

-- instance A.ToJSON SourcePos where
--   toJSON SourcePos{..} =
--     A.toJSON [sourcePosLine, sourcePosColumn]

-- instance A.FromJSON SourcePos where
--   parseJSON arr = do
--     [line, col] <- A.parseJSON arr
--     return $ SourcePos line col

newtype SourceSpan = SourceSpan
  { -- | Source name
    name :: String
    -- | Start of the span
  , start :: SourcePos
    -- | End of the span
  , end :: SourcePos
  }
-- deriving (Show, Read, Eq, Ord, D.Data, D.Typeable)
derive instance genericSourceSpan :: Generic SourceSpan
instance showSourceSpan :: Show SourceSpan where show = gShow

displayStartEndPos :: SourceSpan -> String
displayStartEndPos (SourceSpan sourceSpan) =
  displaySourcePos sourceSpan.start <> " - " <> displaySourcePos sourceSpan.end

displaySourceSpan :: SourceSpan -> String
displaySourceSpan ss@(SourceSpan sourceSpan) =
  sourceSpan.name <> " " <> displayStartEndPos ss

-- instance A.ToJSON SourceSpan where
--   toJSON SourceSpan{..} =
--     A.object [ "name"  .= spanName
--              , "start" .= spanStart
--              , "end"   .= spanEnd
--              ]

-- instance A.FromJSON SourceSpan where
--   parseJSON = A.withObject "SourceSpan" $ \o ->
--     SourceSpan     <$>
--       o .: "name"  <*>
--       o .: "start" <*>
--       o .: "end"

internalModuleSourceSpan :: String -> SourceSpan
internalModuleSourceSpan name =
    let zeroZeroPos = SourcePos { line: 0, column: 0 }
    in SourceSpan { name: name, start: zeroZeroPos, end: zeroZeroPos }

