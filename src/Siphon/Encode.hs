module Siphon.Encode where

import Pipes.Protolude
import Siphon.Types
import Control.Monad
import Data.Vinyl.Core
import Data.Singletons
import Data.Singletons.Prelude
import Data.Traversable (traverse)
import Data.Foldable (traverse_)
import Data.Functor.Contravariant (contramap)

encodeCsv :: (MonadSafe m) => FilePath -> Consumer' (Cell Text) m ()
encodeCsv file = encodeText >-> writeFile file
  where encodeText = await >>= \c -> case c of
          Content t -> do
            yield (t <> ",")
            encodeText
          Newline   -> do
            yield "\n"
            encodeText

-- | Does not include a newline at the end.
yieldRec :: forall (xs :: [k]) t f m. 
     (Monad m)
  => (forall (x :: k). Sing x -> f x -> t) -- ^ rendering function
  -> SList xs -- ^ singletons identifying the fields
  -> Rec f xs -- ^ record
  -> Producer' (Cell t) m ()
yieldRec render = go where 
  go :: forall as. Sing as -> Rec f as -> Producer' (Cell t) m ()
  go SNil RNil = return ()
  go (SCons s sings) (field :& fields) = do
    yield (Content (render s field))
    go sings fields

recs :: forall (xs :: [k]) f t m a. 
     (Monad m) 
  => (forall (x :: k). Sing x -> f x -> t) 
  -> SList xs 
  -> Pipe (Rec f xs) (Cell t) m a
recs render sings = forever $ do
  record <- await
  yieldRec render sings record
  yield Newline

-- cells :: Pipe (Cell t) t m a
-- cells = forever $ do
--   await 

-- | Includes a newline at the end.
yieldRow :: Monad m => Many (Encoding t) d -> d -> Producer' (Cell t) m ()
yieldRow (Many encodings) theData = do
  traverse_ (\(Encoding encode) -> yield (Content (encode theData))) encodings
  yield Newline

-- apply :: Many (Encoding t) d -> d -> Vector t
-- apply (Many encodings) = V.map _ encodings

rows :: Monad m => Many (Encoding t) d -> Pipe d (Cell t) m a
rows encodings = forever $ do
  d <- await
  yieldRow encodings d

headedRows :: Monad m => Many (Headed Encoding content) d -> Pipe d (Cell content) m a
headedRows headedEncodings = do
  forM_ (getMany headedEncodings) $ \(Headed header _) -> yield (Content header)
  let encodings = Many (fmap headedCodec (getMany headedEncodings))
  yield Newline
  rows encodings

withHeader :: content -> Encoding content a -> (b -> a) -> Many (Headed Encoding content) b
withHeader header encoding f = Many (pure (Headed header (contramap f encoding)))



