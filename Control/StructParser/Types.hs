{-# LANGUAGE OverloadedStrings #-}

module Control.StructParser.Types where

import Control.Comonad.Cofree
import Data.Functor.Foldable (Fix (..))
import Text.PrettyPrint.ANSI.Leijen as PP

newtype Identifier = Id
  { unId :: String }
 deriving (Show, Ord, Eq)

data Qualifier
  = InObj Identifier
  | InField String
  | AtIndex Int
    deriving (Show, Ord, Eq)

instance Pretty Qualifier where
  pretty (InObj _)    = empty
  pretty (InField f)  = "in" <+> dot <> text f
  pretty (AtIndex i)  = "at" <+> brackets (int i)

instance Pretty Identifier where
  pretty (Id s) = text s

class GetId a where
  getId :: a -> Identifier
  getIn :: a -> Qualifier
  getIn = InObj . getId

class (Ord k) => FieldKey k where
  fieldQualifier :: k -> Qualifier
  fieldIdentifier :: k -> Identifier

class WithAnnotation f where
  annotate :: Raw f -> Annotated f
  unannotate :: Annotated f -> Raw f

cataAnn :: (Functor f) => (a -> f b -> b) -> Cofree f a -> b
cataAnn alg (ann :< v) = alg ann . fmap (cataAnn alg) $ v

type Raw f = Fix f
type Annotated f = Cofree f (Position, Qualifier)

type Position = [Qualifier]
newtype Context = Context String deriving (Show, Eq, Ord)
