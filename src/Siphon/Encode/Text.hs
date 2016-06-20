module Siphon.Encode.Text where

import Pipes.Protolude
import Siphon.Types
import qualified Data.Text.Lazy             as Lazy
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

int :: Encoding Text Int
int = Encoding (Lazy.toStrict . Builder.toLazyText . Builder.decimal)

text :: Encoding Text Text
text = Encoding id

