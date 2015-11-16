
module Data.ConstIndex where

newtype ConstIndex f k a = ConstIndex { runConstIndex :: f a }

instance (Functor f) => Functor (ConstIndex f a) where
  fmap f (ConstIndex ls) = ConstIndex (fmap f ls)

instance Foldable f => Foldable (ConstIndex f a) where
  foldMap f (ConstIndex ls) = foldMap f ls

instance (Foldable f, Traversable f) => Traversable (ConstIndex f a) where
  traverse f (ConstIndex ls) = fmap ConstIndex (traverse f ls)
