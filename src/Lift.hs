{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

module Lift where

class Lift f i o | f i -> o, o -> f where
  lift' :: f i -> o

instance {-# INCOHERENT #-} f i ~ o => Lift f i o where
  lift' = id
  
instance (Applicative f, Lift f b o', o ~ (f a -> o')) => Lift f (a -> b) o where
  lift' f x = lift' (f <*> x)

lift :: (Applicative f, Lift f i o) => i -> o
lift = lift' . pure

