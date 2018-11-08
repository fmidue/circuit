module Main where

import Graphics.Gnuplot.Simple
import Circuit

-- | impulse response for band pass filter
main = plot_eps "data/BP.eps" bp (impulse (Second 1) (Volt 1)) 10 200
