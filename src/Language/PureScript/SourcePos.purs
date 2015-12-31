
module Language.PureScript.SourcePos where

import Prelude (bind, (<>), show)

-- | Source position information
newtype SourcePos = SourcePos
  { -- | Line number
    line :: Int
    -- | Column number
  , column :: Int
  }
-- deriving (Show, Read, Eq, Ord, D.Data, D.Typeable)

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

