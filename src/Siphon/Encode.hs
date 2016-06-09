module Siphon.Encode where

import Pipes.Protolude

import Siphon.Types

encodeCsv :: (MonadSafe m) => FilePath -> Consumer' (Cell Text) m ()
encodeCsv file = encodeText >-> writeFile file
  where encodeText = await >>= \c -> case c of
          Content t -> do
            yield (t <> ",")
            encodeText
          Newline   -> do
            yield "\n"
            encodeText

