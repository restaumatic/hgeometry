{-# language BangPatterns #-}
{-# language KindSignatures #-}
{-# language DataKinds #-}
{-# language GADTs #-}

module Nat
  ( Nat(..)
  , SingNat(..)
  , ImplicitNat(..)
  , N0
  , N1
  , N2
  , N3
  , N4
  , N5
  ) where

import Data.Kind (Type)

data Nat = Succ !Nat | Zero

data SingNat :: Nat -> Type where
  SingSucc :: !(SingNat n) -> SingNat ('Succ n)
  SingZero :: SingNat 'Zero

class ImplicitNat (n :: Nat) where
  implicitNat :: SingNat n

instance ImplicitNat 'Zero where
  implicitNat = SingZero

instance ImplicitNat n => ImplicitNat ('Succ n) where
  implicitNat = SingSucc implicitNat

type N0 = 'Zero
type N1 = 'Succ N0
type N2 = 'Succ N1
type N3 = 'Succ N2
type N4 = 'Succ N3
type N5 = 'Succ N4
