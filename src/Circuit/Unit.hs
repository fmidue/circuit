-- | units

{-# language GeneralizedNewtypeDeriving #-}

module Circuit.Unit where

import qualified Graphics.Gnuplot.Value.Tuple as T

newtype Resistance = Ohm Double     
  deriving (Eq, Ord, Show, Read, Num, Real)
newtype Capacitance = Farad Double  
  deriving (Eq, Ord, Show, Read, Num, Real)
newtype Inductance = Henry Double   
  deriving (Eq, Ord, Show, Read, Num, Real)
newtype Voltage = Volt Double
  deriving (Eq, Ord, Show, Read, Num, Real, Fractional, T.C)
newtype Current = Ampere Double
  deriving (Eq, Ord, Show, Read, Num, Real, Fractional)
newtype Time = Second Double
  deriving (Eq, Ord, Show, Read, Num, Real, Fractional, Enum, T.C)
newtype Frequency = Hertz Double
  deriving (Eq, Ord, Show, Read, Num, Real, Fractional)

