{-# LANGUAGE DeriveDataTypeable #-}
module Data.Positive where

import Control.Exception
import Data.Aeson
import Data.Binary
import Data.Data
import Data.Ratio
import Data.Text.Prettyprint.Doc
import GHC.Natural

newtype Positive = Positive Natural
  deriving (Eq, Ord, Data)

instance Pretty Positive where
  pretty (Positive n) = pretty n

instance Show Positive where
  showsPrec d (Positive n) = showsPrec d n

instance Binary Positive where
  put (Positive n) = put n
  get = Positive <$> get

instance FromJSON Positive where
  parseJSON v = do
    n <- parseJSON v
    if n > 0
      then pure (Positive n)
      else fail $ show n <> " is not strictly positive"

instance Num Positive where
  Positive x + Positive y = Positive (x + y)
  Positive x - Positive y
    | y < x = Positive (x - y)
    | otherwise = throw Underflow
  Positive 0 * Positive _ = throw Underflow
  Positive _ * Positive 0 = throw Underflow
  Positive x * Positive y = Positive (x * y)
  abs (Positive x) = Positive x
  signum _ = 1
  fromInteger 0 = throw Underflow
  fromInteger x = Positive (fromInteger x)

instance Real Positive where
  toRational (Positive x) = toInteger x % 1

instance Enum Positive where
  toEnum x = let n = x + 1 in fromIntegral n
  fromEnum (Positive n) = fromIntegral n - 1

instance Integral Positive where
  quotRem (Positive x) (Positive y) =
    let (q,r) = quotRem x y in (fromIntegral q, fromIntegral r)
  toInteger (Positive x) = toInteger x

-- | Necessary because @(fromIntegral n) + p@ should be safe, but errors because of possible intermediate @Positive 0@.
plusNat :: Natural -> Positive -> Positive
plusNat n (Positive p) = Positive (n + p)
