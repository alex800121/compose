{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

module Compose where
import Data.Kind (Type)

-- compose :: (a -> b) -> a -> b (i ~ a, o ~ b)
-- compose :: (a -> b) -> (c -> a) -> c -> b (i ~ (c -> a), o ~ (c -> b))
-- compose :: (a -> b) -> (c -> d -> a) -> c -> d -> b (i ~ (c -> d -> a), o ~ (c -> d -> b))

data Nat = Z | S Nat

-- type family HasArgs (a :: Type) b i where
--   HasArgs a b a = b
--   HasArgs a b (c -> d) = c -> HasArgs a b d
--
-- class Compose a b i where
--   compose :: (a -> b) -> i -> HasArgs a b i
--
-- instance {-# INCOHERENT #-} (HasArgs a b i ~ b, a ~ i) => Compose a b i where
--   compose = id
--
-- -- instance (HasArgs a b (c -> d) ~ (c -> HasArgs a b d), Compose a b d) => Compose a b (c -> d) where
-- instance {-# OVERLAPPING #-} (HasArgs a b (c -> d) ~ (c -> HasArgs a b d), Compose a b d) => Compose a b (c -> d) where
--   compose f x = compose f . x

type family CountArgs (a :: Type) :: Nat where
  CountArgs (a -> b) = S (CountArgs b)
  CountArgs b = Z

class HasArgs a b i o | i b -> o, a o -> i, i o -> a b where
  compose :: (a -> b) -> i -> o

instance {-# OVERLAPS #-} (CountArgs a ~ CountArgs i, a ~ i, b ~ o) => HasArgs a b i o where
  compose = id
  
instance {-# OVERLAPS #-} HasArgs a b i o => HasArgs a b (c -> i) (c -> o) where
  compose f x = compose f . x

(.:) :: HasArgs a b i o => (a -> b) -> i -> o
(.:) = compose

-- infixr 8 .:
infixl 9 .:
