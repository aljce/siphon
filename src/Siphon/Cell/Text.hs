module Siphon.Cell.Text where

import Pipes.Protolude hiding (parsed,takeWhile,Parser)
import Data.Attoparsec.Text
import Siphon.Types
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.List as List

-- | This implementation is inefficient. It should be
-- changed later.
-- row :: Vector Text -> Text
-- row = id
--   . V.fromList
--   . (List.++ ["\n"])
--   . List.intersperse "," 
--   . V.fromList
--   . V.map encodeContents
-- 
encodeContents :: Text -> Text
encodeContents t = Text.concat
  [ Text.singleton '"'
  , Text.replace "\"" "\"\"" t
  , Text.singleton '"'
  ]

encodePipe :: Monad m => Pipe (Cell Text) Text m a
encodePipe = go False where
  go previousWasContent = do
    when previousWasContent (yield ",")
    await >>= \case
      Content t -> do
        yield (encodeContents t)
        go True
      Newline -> do
        yield "\n"
        go False

encode :: Cell Text -> Text
encode = \case
  Content t -> Text.concat
    [ Text.singleton '"'
    , Text.replace "\"" "\"\"" t
    , Text.singleton '"'
    ]
  Newline -> Text.singleton '\n'

parser :: Parser (Cell Text)
parser = fmap Content parser' 
     <|> fmap (const Newline) (char '\n')

parser' :: Parser Text
parser' = quotedField <|> unquotedField <?> "field"
  where 
  quotedField   = char '"' *> insideQuotes <* char '"' <?> "quoted field"
  unquotedField = takeWhile (\c -> c /= ',' && c /= '\n')
  insideQuotes  = Text.append 
    <$> takeWhile (/= '"')
    <*> (Text.concat <$> many (Text.cons <$> dQuotes <*> insideQuotes))
    <?> "inside of double quotes"
  dQuotes = string "\"\"" >> return '"' <?> "paired double quotes"


