module Siphon.Misc where

import Pipes.Protolude
import Control.Applicative.Free
import qualified Control.Applicative.Free as Ap

-- | todo: make this strict in the counter
lengthAp :: Ap f a -> Int
lengthAp (Ap.Pure _) = 0
lengthAp (Ap _ apNext) = 1 + lengthAp apNext

