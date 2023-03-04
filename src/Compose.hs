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

class HasArgs a b | a -> b, b -> a where
  apply :: (a -> c) -> b -> c
instance {-# INCOHERENT #-} a ~ b => HasArgs a b where
  apply = id
instance HasArgs a b => HasArgs (c -> a) (c -> b) where
  apply = apply

class Compose a b i o | i b -> o, a o -> i, i o -> a, i o -> b where
  compose :: (a -> b) -> i -> o

instance {-# OVERLAPS #-} (HasArgs a i, b ~ o) => Compose a b i o where
  compose = apply
  
instance {-# OVERLAPS #-} Compose a b i o => Compose a b (c -> i) (c -> o) where
  compose f x = compose f . x

(.:) :: Compose a b i o => (a -> b) -> i -> o
(.:) = compose

-- infixr 8 .:
infixl 9 .:
