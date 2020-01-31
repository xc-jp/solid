module Data.Positive where

import Control.Exception
import Data.Aeson
import Data.Ratio
import GHC.Natural

newtype Positive = Positive Natural
  deriving (Show, Eq, Ord)

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
