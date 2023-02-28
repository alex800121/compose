{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE NoImplicitPrelude #-}

module Compose where
import Data.Kind (Type)

-- compose :: (a -> b) -> a -> b (i ~ a, o ~ b)
-- compose :: (a -> b) -> (c -> a) -> c -> b (i ~ (c -> a), o ~ (c -> b))
-- compose :: (a -> b) -> (c -> d -> a) -> c -> d -> b (i ~ (c -> d -> a), o ~ (c -> d -> b))

data Nat = Z | S Nat

type family CountArgs a where
  CountArgs (c -> a) = S (CountArgs a)
  CountArgs a = Z

class Reduce a b
instance Reduce a a
instance Reduce a b => Reduce a (c -> b)

class Compose a b i o | i -> a, o -> b where
  compose :: (a -> b) -> i -> o

instance {-# OVERLAPS #-} Compose a b a b where
  compose = id

instance Compose a b i o => Compose a b (c -> i) (c -> o) where
  compose f x c = compose f (x c)

head' :: String -> Char
head' = head

show' :: String -> String
show' = show
