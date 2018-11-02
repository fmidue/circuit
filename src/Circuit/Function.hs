-- | input functions

module Circuit.Function where

import Circuit.Unit

sine :: Frequency -> Time -> Voltage
sine f t = realToFrac $ sin $ 2 * pi * realToFrac f * realToFrac t

rect :: Frequency -> Time -> Voltage
rect f t = signum $ sine f t

impulse :: Time -> Voltage -> Time -> Voltage
impulse width height = \ t -> if t < width then height else 0


