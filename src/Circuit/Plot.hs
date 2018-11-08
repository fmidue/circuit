module Circuit.Plot where

import Circuit.Unit
import Circuit.Data
import Circuit.Function
import Circuit.Simulate

import Graphics.Gnuplot.Simple

plot = plot_with []
plot_png f = plot_with [ PNG f ]
plot_eps f = plot_with [ EPS f ]

plot_with :: [Attribute] -> Circuit -> (Time -> Voltage) -> Time -> Int -> IO ()
plot_with atts c f total steps = do
  let delta = total / fromIntegral steps
      inputs = take steps $  map (\ t -> (t, f t)) [ 0, delta .. ]
      outputs = take steps $ apply_func c delta f
  plotListsStyle (atts <> [ XLabel "Time (s)", YLabel "Voltage (V)" ] )
    [ ( PlotStyle Lines $ CustomStyle [ LineTitle "input" ], inputs )
    , ( PlotStyle Lines $ CustomStyle [ LineTitle "output" ], outputs )
    ]
