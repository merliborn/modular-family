{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Data.Modular.Some (
    FinNat(FNat)
  , zero, boundSing
) where

import GHC.TypeNats(        -- base >= 4.18.0.0
    Natural
  , Nat
  , KnownNat(natSing), natVal
  , SomeNat, someNatVal
  , type (<=)
  , SNat, pattern SNat
  , fromSNat
  , withSomeSNat
  )
import Data.Kind  (Type)    -- base >= 4.9.0.0
import Data.Proxy (Proxy(Proxy))   -- base >= 4.7.0.0

import GHC.Num    (Num(..))
import Data.Eq    (Eq(..))
import Data.Ord   (Ord(..))
import GHC.Enum   (Bounded(..))
import Data.Function (
  )
import GHC.Real   ()
import GHC.Err    (undefined)

data FinNat (n :: Nat) = FNat Natural (SNat n)

zero :: forall n. (KnownNat n, (1::Nat) <= n) => FinNat n
zero = FNat 0 natSing
boundSing :: FinNat n -> SNat n
boundSing (FNat _ x) = x
upcast :: forall m n. (KnownNat n, KnownNat m, m <= n) => FinNat m -> FinNat n
upcast (FNat i _) = FNat i (SNat @n)

instance Eq (FinNat n) where
  FNat x _ == FNat y _ = x == y
instance Ord (FinNat n) where
  FNat x _ <= FNat y _ = x <= y
instance (KnownNat n, (1::Nat) <= n) => Bounded (FinNat n) where
  minBound = zero
  maxBound = FNat mo natSing
    where
      mo = natVal (Proxy :: Proxy n) - 1

