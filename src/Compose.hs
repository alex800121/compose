{-# LANGUAGE TypeFamilies #-}

module Compose where

-- compose :: (a -> b) -> a -> b (i ~ a, o ~ b)
-- compose :: (a -> b) -> (c -> a) -> c -> b (i ~ (c -> a), o ~ (c -> b))
-- compose :: (a -> b) -> (c -> d -> a) -> c -> d -> b (i ~ (c -> d -> a), o ~ (c -> d -> b))

type family HasArgs a b i where
  HasArgs a b a = b
  HasArgs a b (c -> d) = c -> HasArgs a b d

class Compose a b i where
  compose :: (a -> b) -> i -> HasArgs a b i

instance {-# INCOHERENT #-} (HasArgs a b i ~ b, a ~ i) => Compose a b i where
  compose = id

instance {-# OVERLAPS #-} (HasArgs a b (c -> d) ~ (c -> HasArgs a b d), Compose a b d) => Compose a b (c -> d) where
  compose f x y = compose f (x y)

(.:) :: Compose a b i => (a -> b) -> i -> HasArgs a b i
(.:) = compose

infixr 9 .:
