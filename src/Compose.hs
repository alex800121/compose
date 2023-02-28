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

type family R f i o where
  R (a -> b) a b = a -> b
  R f (c -> i) (c -> o) = R f i o

class (R f i o ~ f) => Reduce f i o where
  compose :: f -> i -> o
instance (b ~ o) => Reduce (a -> b) a o where
  compose = id
instance (R (a -> b) i o ~ R (a -> b) (c -> i) (d -> o), Reduce (a -> b) i o, c ~ d) => Reduce (a -> b) (c -> i) (d -> o) where
  compose f i x = compose f (i x)
  

head' :: String -> Char
head' = head
