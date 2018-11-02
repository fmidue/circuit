-- | units

{-# language GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Circuit.Unit where

import qualified Graphics.Gnuplot.Value.Tuple as T
import GHC.Generics

newtype Resistance = Ohm Double     
  deriving (Eq, Ord, Show, Read, Num, Real, Generic)
newtype Capacitance = Farad Double  
  deriving (Eq, Ord, Show, Read, Num, Real, Generic)
newtype Inductance = Henry Double   
  deriving (Eq, Ord, Show, Read, Num, Real, Generic)
newtype Voltage = Volt Double
  deriving (Eq, Ord, Show, Read, Num, Real, Fractional, T.C, Generic)
newtype Current = Ampere Double
  deriving (Eq, Ord, Show, Read, Num, Real, Fractional, Generic)
newtype Time = Second Double
  deriving (Eq, Ord, Show, Read, Num, Real, Fractional, Enum, T.C, Generic)
newtype Frequency = Hertz Double
  deriving (Eq, Ord, Show, Read, Num, Real, Fractional, Generic)

