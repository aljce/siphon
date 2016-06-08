{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Siphon.Test where

import Pipes.Protolude

import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TH

import Data.Vinyl
import qualified Data.Text as T

import Text.Read (readMaybe)

import Siphon.Parser
import Siphon.Types

data Fields = Name | Age | Sleeping deriving Show

type LifeForm = [Name,Age,Sleeping]

type family ElF (f :: Fields) :: * where
  ElF Name = String
  ElF Age  = Int
  ElF Sleeping = Bool

newtype Attr f = Attr { unAttr :: ElF f }

instance Show (Attr Name) where show (Attr x) = "name: " <> show x
instance Show (Attr Age) where show (Attr x) = "age: " <> show x
instance Show (Attr Sleeping) where show (Attr x) = "sleeping: " <> show x

genSingletons [ ''Fields ]

lf :: SList LifeForm
lf = SCons SName (SCons SAge (SCons SSleeping SNil))

fromFields :: SFields x -> Attr x -> Text
fromFields SName (Attr x) = T.pack x
fromFields SAge (Attr x) = showt x
fromFields SSleeping (Attr x) = showt x

toFields :: Text -> Sing x -> Either String (Attr x)
toFields text SName = return (Attr (T.unpack text))
toFields text SAge  = maybeToRight "No Parse" $
  Attr <$> (readMaybe (T.unpack text) :: Maybe Int)
toFields text SSleeping = maybeToRight "No Parse" $
  Attr <$> (readMaybe (T.unpack text) :: Maybe Bool)

liftErrors :: (MonadIO m) => Pipe (Either e a) a (ExceptT e m) b
liftErrors = do
  val <- await
  case val of
    Left e -> throwError e
    Right a -> do
      yield a
      liftErrors

testing :: IO ()
testing = do
  res <- runSafeT $ runExceptT $ runEffect $
           hoist (withExceptT show) (parseCsvFile dataPath2) >->
           toCell toFields lf >-> liftErrors >-> fromCell fromFields lf >->
           map FromStringShow >-> print
  printT (FromStringShow res)
