module Siphon.Types where

import Pipes.Protolude

data Cell a = Content a | Newline deriving (Functor,Show)

newtype Parsing s a = Parsing { getParsing :: s -> Either Text a }

dataPath = "/home/kyle/Downloads/SacramentocrimeJanuary2006.csv"
dataPath2 = "/home/kyle/repos/haskell/siphon/data.csv"
