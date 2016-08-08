{-# LANGUAGE GADTs #-}
module PB
    (
    ) where

import Control.Applicative (Alternative(..))

data PB a where
    Pure :: a -> PB a
    Ap :: PB (a -> b) -> PB a -> PB b
    Or :: PB a -> PB a -> PB a
    Many :: PB a -> PB [a]
    Empty :: PB a

instance Functor PB where
    fmap f (Pure x) = Pure (f x)
    fmap f (Ap pf px) = Ap ((f .) <$> pf) px

instance Applicative PB where
    pure = Pure
    (<*>) = Ap

instance Alternative PB where
    empty = Empty
    (<|>) = Or
    many = Many
