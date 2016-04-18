-- | Convenient operator.

module Data.Monoid.Operator where



(++) :: (Monoid a) => a -> a -> a
(++) = mappend
