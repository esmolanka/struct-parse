{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.StructParser.Types where

import Control.Comonad.Cofree
import Data.Functor.Foldable (Fix (..))
import Data.Text as T
import Text.PrettyPrint.ANSI.Leijen as PP

data Qualifier
  = QObject String
  | QField String
  | QIndex Int
    deriving (Show, Ord, Eq)

instance Pretty Qualifier where
  pretty (QObject s) = text s
  pretty (QField f)  = dot <> text f
  pretty (QIndex i)  = brackets (int i)

class GetId a where
  getId :: a -> Qualifier

class (Ord k) => FieldKey k where
  fieldQualifier :: k -> Qualifier

instance FieldKey [Char] where
  fieldQualifier = QField

instance FieldKey Text where
  fieldQualifier s = QField (T.unpack s)

class WithAnnotation f where
  annotate :: Raw f -> Annotated f
  unannotate :: Annotated f -> Raw f

cataAnn :: (Functor f) => (a -> f b -> b) -> Cofree f a -> b
cataAnn alg (ann :< v) = alg ann . fmap (cataAnn alg) $ v

type Raw f = Fix f
type Annotated f = Cofree f (Position, Qualifier)

type Position = [Qualifier]
newtype Context = Context String deriving (Show, Eq, Ord)
