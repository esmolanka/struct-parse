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

newtype Codensity f a = Codensity
  { runCodensity :: forall r. (a -> f r) -> f r }

instance (Functor f) => Functor (Codensity f) where
  fmap f c = Codensity (\k -> runCodensity c (k . f))

instance (Applicative f, Monad f) => Applicative (Codensity f) where
  pure a = Codensity $ \k -> k a
  (<*>) kfa kb = Codensity $ \k -> (runCodensity kfa pure <*> runCodensity kb pure) >>= k

instance (Alternative f, Monad f) => Alternative (Codensity f) where
  empty = Codensity $ \_ -> empty
  (<|>) ka kb = Codensity $ \k -> (runCodensity ka pure <|> runCodensity kb pure) >>= k

instance (Monad f) => Monad (Codensity f) where
  return a = Codensity $ \k -> k a
  (>>=) ka fkb = Codensity $ \k -> runCodensity ka $ \a -> runCodensity (fkb a) k

newtype Parsing f a b = Parsing
  { runParsing' :: a -> Codensity f b }

runParsing :: (Applicative f) => Parsing f a b -> a -> f b
runParsing p a = runCodensity (runParsing' p a) pure

instance Functor f => Functor (Parsing f a) where
  fmap f p = Parsing $ \i -> fmap f (runParsing' p i)

instance (Applicative f, Monad f) => Applicative (Parsing f a) where
  pure a = Parsing $ \_ -> pure a
  (<*>) pfa pb = Parsing $ \i -> runParsing' pfa i <*> runParsing' pb i

instance (Alternative f, Monad f) => Alternative (Parsing f a) where
  empty = Parsing $ \_ -> empty
  (<|>) pa pb = Parsing $ \i -> runParsing' pa i <|> runParsing' pb i

instance (Monad f) => Category (Parsing f) where
  id = Parsing $ \i -> Codensity ($ i)
  (.) pbc pab = Parsing $ \i -> runParsing' pab i >>= runParsing' pbc

instance (Applicative f, Monad f) => Arrow (Parsing f) where
  arr f = Parsing $ \i -> Codensity $ \k -> k (f i)
  first p = Parsing $ \(i, c) -> (,) <$> runParsing' p i <*> pure c
  second p = Parsing $ \(c, i) -> (,) <$> pure c <*> runParsing' p i
  (***) pa pb = Parsing $ \(a, b) -> (,) <$> runParsing' pa a <*> runParsing' pb b
  (&&&) pa pb = Parsing $ \i -> (,) <$> runParsing' pa i <*> runParsing' pb i

instance (Alternative f, Monad f) => ArrowZero (Parsing f) where
  zeroArrow = empty

instance (Alternative f, Monad f) => ArrowPlus (Parsing f) where
  (<+>) = (<|>)

type Parseable f = (Alternative f, Applicative f, Monad f)

class Hole f a where
  holes :: Parseable f => Parsing f a [a]

mkParsing :: (Parseable f) => (forall r. (b -> f r) -> a -> f r) -> Parsing f a b
mkParsing with = Parsing $ \i -> Codensity $ \f -> with f i

traversing :: (Parseable f, Traversable t) => Parsing f a b -> Parsing f (t a) (t b)
traversing p = Parsing $ \i -> T.traverse (runParsing' p) i

traversing' :: (Parseable f, Traversable t) => Parsing f a b -> Parsing f (t a) (t b)
traversing' p = Parsing $ \i -> T.mapM (runParsing' p) i

recursive :: (Parseable f) => Parsing f a [a] -> Parsing f a b -> Parsing f a [b]
recursive holes extract = arr (:[]) . extract <|> arr concat . traversing' (recursive holes extract) . holes

infixr 0 *>>
(*>>) :: (Parseable f, Hole f b) => Parsing f a b -> Parsing f b c -> Parsing f a [c]
(*>>) p extract = p >>> recursive holes extract

infixr 0 <<*
(<<*) :: (Parseable f, Hole f b) => Parsing f b c -> Parsing f a b -> Parsing f a [c]
(<<*) extract p = recursive holes extract <<< p
