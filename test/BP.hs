module Main where

import Graphics.Gnuplot.Simple
import Circuit

-- | impulse response for band pass filter
main = plotList [ EPS "data/BP.eps" ] 
     $ take 1000 
     $ apply_func bp (Second 0.02)
     $ impulse (Second 0.1) (Volt 1)
