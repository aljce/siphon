module Siphon.Types where

import Prelude (Read)
import Pipes.Protolude
import Data.Functor.Contravariant
import Data.Profunctor
import Data.Semigroup (Semigroup)
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
import qualified Data.Vector as V
import Control.Monad.Morph

data Cell a = Content !a | Newline
  deriving (Eq,Ord,Functor,Show,Read)

-- | Strategy for rendering a column with a header
--   in a CSV file.
data Column content a = Column
  { columnHeader :: !content
  , columnRender :: !(a -> content)
  }

data Headed f content a = Headed
  { headedHeader :: !content
  , headedCodec  :: !(f content a)
  }

newtype Encoding content a = Encoding { getEncoding :: a -> content }
newtype Decoding content a = Decoding { getDecoding :: content -> Either String a }
  deriving (Functor)

instance Applicative (Decoding content) where
  pure a = Decoding (const (Right a))
  Decoding f <*> Decoding g = Decoding $ \content -> f content <*> g content

instance Contravariant (Encoding content) where
  contramap f (Encoding go) = Encoding (go . f)

-- instance Profunctor Decoding where
--   dimap f g (Decoding go) = Decoding (fmap g . go . f)

-- instance Profunctor f => Profunctor (Headed f) where
--   dimap f g (Headed header codec) = Headed (g header) (dimap f g codec)

-- dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

-- | This typeclass should be moved into another library. I need to
--   think about what this should actually be called. Hmmm...
--   Maybe something like ContravariantSkip1. Not sure.
class ContravariantConst1 f where
  contramapConst1 :: (a -> b) -> f c b -> f c a

class Functor1 f where
  fmap1 :: (forall a. g a -> h a) -> f g -> f h

class FunctorConst1 f where
  fmapConst1 :: (a -> b) -> f c a -> f c b

instance FunctorConst1 Decoding where
  fmapConst1 = fmap

instance ContravariantConst1 Encoding where
  contramapConst1 = contramap

instance ContravariantConst1 f => Contravariant (Headed f content) where
  contramap g (Headed header codec) = Headed header (contramapConst1 g codec)

instance FunctorConst1 f => Functor (Headed f content) where
  fmap g (Headed header codec) = Headed header (fmapConst1 g codec)

-- instance Applicative (Headed f content) where
--   pure 

instance Contravariant (Column content) where
  contramap g (Column h c) = Column h (c . g)

-- | Basically, a newtype wrapper around 'Vector' that gives 
--   us a 'Contravariant' instance.
newtype Many f a = Many {getMany :: V.Vector (f a)}
  deriving (Monoid,Functor,Traversable,Foldable)

-- newtype Grouped f a = Grouped  {getGrouped :: f a -> }

-- alpha :: Many (Headed Decoding Text) Int 
--       -> Many (Headed Decoding Text) Char
--       -> Many (Headed Decoding Text) (Int,Char)

instance MFunctor Many where
  hoist f (Many v) = Many (V.map f v)

instance Contravariant f => Contravariant (Many f) where
  contramap f (Many v) = Many (V.map (contramap f) v)


newtype Columns content a = Columns (V.Vector (Column content a))
  deriving (Monoid)

instance Contravariant (Columns content) where
  contramap g (Columns cols) = Columns (fmap (contramap g) cols)

newtype Parsing s a = Parsing { getParsing :: s -> Either Text a }

dataPath = "/home/kyle/Downloads/SacramentocrimeJanuary2006.csv"
dataPath2 = "/home/kyle/repos/haskell/siphon/data.csv"
