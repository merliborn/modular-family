{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoStarIsType #-}

module Data.Modular.Some.Exception (
    FinNatException(
      NonPositiveUpperBound,
      ValueOverflow,
      ValueUnderflow
    )
  , Exception (toException,fromException,displayException)
  , throw
) where

import Control.Exception (
    SomeException (SomeException)
  , Exception (toException,fromException,displayException)
  , throw
  )

import Data.Eq    (Eq(..))
import Data.Ord   (Ord(..))
import Text.Show  (Show(..))

data FinNatException
  = NonPositiveUpperBound
  | ValueOverflow
  | ValueUnderflow
  deriving (Eq, Ord)

nonPositiveUpperBound, valueOverflow, valueUnderflow :: SomeException
nonPositiveUpperBound = toException NonPositiveUpperBound
valueOverflow         = toException ValueOverflow
valueUnderflow        = toException ValueUnderflow

instance Exception FinNatException
instance Show FinNatException where
  show NonPositiveUpperBound  = show "non-positive bound parameter"
  show ValueOverflow          = show "value exceeding the bound"
  show ValueUnderflow         = show "value underflow (negative value)"