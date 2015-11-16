{-# LANGUAGE RankNTypes, MultiParamTypeClasses, ConstraintKinds #-}

module Control.Parsing (
  -- * Construct and run Parsing
    Parsing
  , mkParsing
  , runParsing
  -- * Parseable underlying typeclass
  , Parseable
  , Hole (..)
  -- * Combinators
  , traversing
  , traversing'
  , recursive
  , (<<*), (*>>)
  , (<<^), (^>>)
  , (<<<), (>>>)
  ) where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Applicative
import Control.Category

import Data.Traversable as T (traverse, mapM)

newtype Parsing f a b = Parsing
  { runParsing :: a -> f b }

instance Functor f => Functor (Parsing f a) where
  fmap f p = Parsing $ \i -> fmap f (runParsing p i)

instance (Applicative f) => Applicative (Parsing f a) where
  pure a = Parsing $ \_ -> pure a
  (<*>) pfa pb = Parsing $ \i -> runParsing pfa i <*> runParsing pb i

instance (Alternative f) => Alternative (Parsing f a) where
  empty = Parsing $ \_ -> empty
  (<|>) pa pb = Parsing $ \i -> runParsing pa i <|> runParsing pb i

instance (Monad f) => Category (Parsing f) where
  id = Parsing $ \i -> return i
  (.) pbc pab = Parsing $ \i -> runParsing pab i >>= runParsing pbc

instance (Applicative f, Monad f) => Arrow (Parsing f) where
  -- Monad f constaint is needed because Category instance needs it.
  arr f = Parsing $ \i -> pure (f i)
  first p = Parsing $ \(i, c) -> (,) <$> runParsing p i <*> pure c
  second p = Parsing $ \(c, i) -> (,) <$> pure c <*> runParsing p i
  (***) pa pb = Parsing $ \(a, b) -> (,) <$> runParsing pa a <*> runParsing pb b
  (&&&) pa pb = Parsing $ \i -> (,) <$> runParsing pa i <*> runParsing pb i

instance (Alternative f, Monad f) => ArrowZero (Parsing f) where
  zeroArrow = empty

instance (Alternative f, Monad f) => ArrowPlus (Parsing f) where
  (<+>) = (<|>)

type Parseable f = (Alternative f, Applicative f, Monad f)

class Hole f a where
  holes :: Parseable f => Parsing f a [a]

mkParsing :: (Parseable f) => (forall r. (b -> f r) -> a -> f r) -> Parsing f a b
mkParsing with = Parsing $ \i -> with pure i

traversing :: (Parseable f, Traversable t) => Parsing f a b -> Parsing f (t a) (t b)
traversing p = Parsing $ \i -> T.traverse (runParsing p) i

traversing' :: (Parseable f, Traversable t) => Parsing f a b -> Parsing f (t a) (t b)
traversing' p = Parsing $ \i -> T.mapM (runParsing p) i

recursive :: (Parseable f) => Parsing f a [a] -> Parsing f a b -> Parsing f a [b]
recursive holes extract = arr (:[]) . extract <|> arr concat . traversing' (recursive holes extract) . holes

infixr 0 *>>
(*>>) :: (Parseable f, Hole f b) => Parsing f a b -> Parsing f b c -> Parsing f a [c]
(*>>) p extract = p >>> recursive holes extract

infixr 0 <<*
(<<*) :: (Parseable f, Hole f b) => Parsing f b c -> Parsing f a b -> Parsing f a [c]
(<<*) extract p = recursive holes extract <<< p
