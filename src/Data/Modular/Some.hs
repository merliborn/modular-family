{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE GADTs #-}

module Data.Modular.Some (
    FinNat(FNat)
  , zero, boundSing
  , SomeFinNat, someFinNat
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
import GHC.Real   (Integral(toInteger))

import Data.Bool
import Data.Maybe (Maybe(..))

import Data.Eq    (Eq(..))
import Data.Ord   (Ord(..))
import GHC.Enum   (Bounded(..))
import Data.Function (
    ($), (.)
  )
import GHC.Real   (
    fromIntegral
  )
import GHC.Err    (undefined)

data FinNat (n :: Nat) = FNat Natural (SNat n)

zero :: forall n. (KnownNat n, (1::Nat) <= n) => FinNat n
zero = FNat 0 natSing
boundSing :: FinNat n -> SNat n
boundSing (FNat _ x) = x
upcast :: forall m n. (KnownNat n, KnownNat m, m <= n) => FinNat m -> FinNat n
upcast (FNat i _) = FNat i (SNat @n)

data SomeFinNat = forall n. SomeFinNat (FinNat n)
someFinNat :: (Integral a, Integral b) => a -> b -> Maybe SomeFinNat
someFinNat i n | n <= 0                     = Nothing
               | i <  0                     = Nothing
               | toInteger n <= toInteger i = Nothing
               | otherwise                  = Just $ withSomeSNat (fromIntegral n) (\sn->SomeFinNat (FNat (fromIntegral i) sn))
toFinNat :: forall a (n::Nat). (Integral a, KnownNat n) => a -> Maybe (FinNat n)
toFinNat i | i < 0                                                = Nothing
           | toInteger (natVal (Proxy :: Proxy n)) <= toInteger i = Nothing
           | otherwise
              = Just $ FNat (fromIntegral i) (SNat @n)

instance Eq (FinNat n) where
  FNat x _ == FNat y _ = x == y
instance Ord (FinNat n) where
  FNat x _ <= FNat y _ = x <= y
instance (KnownNat n, (1::Nat) <= n) => Bounded (FinNat n) where
  minBound = zero
  maxBound = FNat mo natSing
    where
      mo = natVal (Proxy :: Proxy n) - 1

