module Siphon.Decode where

import Pipes.Protolude hiding (parsed,takeWhile)

import Data.Singletons
import Data.Singletons.Prelude
import Data.Vinyl

import Pipes.Attoparsec (parsed,ParsingError)
import Data.Attoparsec.Text hiding (take)
import qualified Data.Text as T

import Control.Lens (view)
import Control.Applicative.Free
import qualified Control.Applicative.Free as Ap
import qualified Data.Vector as V

import Siphon.Types

parseCell = quotedField <|> unquotedField <?> "field"
  where quotedField   = char '"' *> insideQuotes <* char '"' <?> "quoted field"
        unquotedField = takeWhile (\c -> c /= ',')
        insideQuotes  = T.append <$> takeWhile (/= '"')
                                 <*> (T.concat <$> many (T.cons <$> dQuotes <*> insideQuotes))
                                 <?> "inside of double quotes"
        dQuotes = string "\"\"" >> return '"' <?> "paired double quotes"

parseCsv = parseCell `sepBy` char ','

parseLine :: (Monad m) => Producer Text m r -> Producer (Cell Text) (ExceptT ParsingError m) r
parseLine = toCells . exceptP . fmap (first fst) . parsed parseCsv
  where toCells p = p >-> concat >-> map Content

getLines :: (MonadSafe m) => String -> FreeT (Producer (Cell Text) (ExceptT ParsingError m)) m ()
getLines = maps parseLine . view lines . readFile

parseCsvFile :: (MonadSafe m) => String -> Producer (Cell Text) (ExceptT ParsingError m) ()
parseCsvFile str = do
  intercalates (yield Newline) (hoistFreeT lift $ getLines str)
  yield Newline

toCell :: forall (as :: [k]) f t m a. (Monad m) =>
           (forall (x :: k). t -> Sing x -> Either String (f x)) ->
           SList as -> Pipe (Cell t) (Either String (Rec f as)) m a
toCell trans tags = do
  record <- go tags
  yield record
  toCell trans tags
  where go :: forall (bs :: [k]). SList bs -> Consumer' (Cell t) m (Either String (Rec f bs))
        go SNil = do
          cell <- await
          case cell of
            Content _ -> return (throwError "Too much input")
            Newline   -> return (return RNil)
        go (SCons headS tailS) = do
          cell <- await
          case cell of
            Content t -> do
              recordE <- go tailS
              return (do headR <- trans t headS
                         tailR <- recordE
                         return (headR :& tailR))
            Newline -> return (throwError "Not enough input")


fromCell :: forall (xs :: [k]) f t m a. (Monad m) =>
           (forall (x :: k). Sing x -> f x -> t) -> SList xs -> Pipe (Rec f xs) (Cell t) m a
fromCell trans tags = do
  record <- await
  go record tags
  fromCell trans tags
  where go :: forall (as :: [k]). Rec f as -> SList as -> Producer' (Cell t) m ()
        go RNil SNil = yield Newline
        go (field :& restR) (SCons singS restS) = do
          yield (Content (trans singS field))
          go restR restS

analyzeDecoding :: forall content a.
     Ap (Decoding content) a 
  -> V.Vector content
  -> Either String a
analyzeDecoding a v = go 0 a
  where
  go :: forall b. Int -> Ap (Decoding content) b -> Either String b
  go _   (Ap.Pure x) = Right x
  go !ix (Ap (Decoding f) apNext) = case v V.!? ix of
    Nothing -> Left "ran out of cells"
    Just content -> do
      a <- f content
      g <- go (ix + 1) apNext
      Right (g a)

test5 :: IO ()
test5 = do
  res <- runSafeT $ runExceptT $ runEffect $ parseCsvFile dataPath2 >-> map FromStringShow >-> print
  printT (FromStringShow res)
