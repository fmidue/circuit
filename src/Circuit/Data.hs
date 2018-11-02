-- | circuits and components

{-# language GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Circuit.Data where

import Circuit.Unit

import qualified Data.Map.Strict as M

import Graphics.Gnuplot.Simple (plotList, plotLists)

import GHC.Generics


newtype Node = Node Int deriving (Eq, Ord, Show, Read, Enum, Num, Generic)
newtype Edge = Edge Int deriving (Eq, Ord, Show, Read, Enum, Num, Generic)

data Circuit = Circuit
  { ground :: Node
  , output :: Node
  , components :: M.Map Edge (Node, Component, Node)
  } deriving (Show, Generic)

data Component
  = Resistor Resistance
  | Capacitor Capacitance
  | Inductor Inductance
  | Current_Source Current
  | Voltage_Source Voltage
  | Voltage_Source_Input
  | Opamp { pos :: Node, neg :: Node }
  deriving (Show, Generic)


-- * example circuits

-- | Spannungsteiler
rr :: Circuit
rr = Circuit
  { ground = 0, output = 2
  , components = fromList
    [ (1, Voltage_Source_Input, 0)
    , (1, Resistor  (Ohm   1), 2)
    , (2, Resistor  (Ohm   1), 0)
    ]
  }

-- | Tiefpaß
lp :: Circuit
lp = Circuit
  { ground = 0, output = 2
  , components = fromList
    [ (1, Voltage_Source_Input, 0)
    , (1, Resistor  (Ohm   1), 2)
    , (2, Capacitor (Farad 1), 0)
    ]
  }

-- | band pass
bp :: Circuit
bp = Circuit
  { ground = 0, output = 2
  , components = fromList
    [ (1, Voltage_Source_Input, 0)
    , (1, Resistor  (Ohm   1), 2)
    , (2, Capacitor (Farad 1), 0)
    , (2, Inductor (Henry 1), 0)
    ]
  }

-- | Bridged T delay equaliser
btde :: Circuit
btde = Circuit
  { ground = 0, output = 3
  , components = fromList
    [ (1, Voltage_Source_Input, 0)
    , (1, Inductor (Henry 1), 2)
    , (2, Inductor (Henry 1), 3)
    , (1, Capacitor (Farad 1), 3)
    , (2, Capacitor (Farad 1), 4)
    , (4, Inductor (Henry 1), 0)
    ]
  }

-- | inverting amplifier
ia :: Circuit
ia = Circuit
  { ground = 0, output = 3
  , components = fromList
    [ (1, Voltage_Source_Input, 0)
    , (1, Resistor (Ohm 1), 2)
    , (2, Resistor (Ohm 2), 3)
    , (3, Opamp { pos = 0, neg = 2 }, 0 )
    ]
  }

-- | op amp all pass
ap :: Circuit
ap = Circuit
  { ground = 0, output = 4
  , components = fromList
    [ (1, Voltage_Source_Input, 0)
    , (1, Capacitor (Farad 1), 2)
    , (2, Resistor (Ohm 1), 0)
    , (1, Resistor (Ohm 1), 3)
    , (3, Resistor (Ohm 1), 4)
    , (4, Opamp { pos = 2, neg = 3 }, 0 )
    ]
  }

-- | phase shift oscillator (no input)
pso :: Circuit
pso = Circuit
  { ground = 0, output = 5
  , components = fromList
    [ (2, Resistor (Ohm 1), 3), (3, Capacitor (Farad 1), 0)
    , (3, Resistor (Ohm 1), 4), (4, Capacitor (Farad 1), 0)
    , (4, Resistor (Ohm 1), 5), (5, Capacitor (Farad 1), 0)
    , (5, Resistor (Ohm 5), 6)
    , (6, Resistor (Ohm 100), 2)
    , (1, Voltage_Source (Volt 1), 0)
    , (2, Opamp { pos = 1, neg = 6 }, 0)
    ]
  }

-- * utility functions for building and using circuits

fromList xs = M.fromList $ zip [ 0 .. ] xs

edges :: Circuit -> [ (Edge, (Node, Component, Node)) ]
edges c = M.toList $ components c




  

