-- | external API for calling the simulator

module Circuit.Simulate where

import Circuit.Unit
import Circuit.Data
import Circuit.Vector

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad (guard, mzero)
import Data.List (mapAccumL)

import Data.Monoid ((<>))

apply :: Circuit -> Time -> [ Voltage ] -> [ Voltage ]
apply c delta inputs =
  snd $ mapAccumL (step c delta) (initial c) inputs

apply_func :: Circuit -> Time -> (Time -> Voltage) -> [ (Time, Voltage) ]
apply_func c delta f = 
  let ts = [0, delta .. ]
  in  zip ts $ apply c delta $ map f ts

opamp_gain = 1e3 :: Double


data State = State
  { voltage :: M.Map Node Voltage
  , current :: M.Map Edge Current
  } deriving Show

data Sources = Sources (M.Map Edge (Node, Component, Node) )
  deriving Show

step :: Circuit -> Time -> Sources -> Voltage -> ( Sources, Voltage )
step c delta s inp =
  let v = solve Unit $ kirchhoff_equations ( with_sources c s ) inp
      st = State { voltage = M.fromList $ do
                (V p, u) <- assocs v
                return (p, Volt u)
            , current = M.fromList $ do
                (C e, i) <- assocs v
                return (e, Ampere i)
            }
  in  ( update c delta s st , voltage st M.! output c )

with_sources :: Circuit -> Sources -> Circuit
with_sources c (Sources s) =
  c { components = M.union s $ components c }

initial :: Circuit -> Sources
initial c =  Sources $ M.fromList $ do
  (e, (p, comp, q)) <- edges c
  case comp of
    Capacitor cap ->
      return (e, (p, Voltage_Source 0, q))
    Inductor ind ->
      return (e, (p, Current_Source 0, q))
    _ -> mzero

-- | change state for time-dependent components (capacitor, inductor)
update :: Circuit -> Time -> Sources -> State -> Sources
update c delta (Sources s) x = Sources $ M.fromList $ do
  (e, (p, comp, q)) <- edges c
  case comp of
    Capacitor c ->
      let (_, Voltage_Source u, _) = s M.! e
          du = scale_cap delta c ( current x M.! e ) 
      in  return (e, (p, Voltage_Source $ u + du, q))
    Inductor l -> 
      let (_, Current_Source i, _) = s M.! e
          di = scale_ind delta l ( voltage x M.! p - voltage x M.! q ) 
      in  return (e, (p, Current_Source $ i + di, q))
    _ -> mzero

scale_cap :: Time -> Capacitance -> Current -> Voltage
scale_cap t c i =
  realToFrac ( realToFrac t / realToFrac c * realToFrac i :: Double )

scale_ind :: Time -> Inductance -> Voltage -> Current 
scale_ind t l v =
  realToFrac ( realToFrac t / realToFrac l * realToFrac v :: Double )


-- * determine currents and voltages in a resistor circuit

data Unknown = V Node | C Edge | Unit
  deriving (Eq, Ord, Show)

kirchhoff_equations :: Circuit -> Voltage -> [ Equation Unknown ]
kirchhoff_equations c inp =
  let for_edges = do
        (e, (p, comp, q)) <- edges c
        return $ case comp of
          Resistor r ->
            equals [ (V p, 1), (V q, -1)] [ ( C e, realToFrac r) ]
          Current_Source i ->
            equals [ (C e, 1)] [ (Unit, realToFrac i) ]
          Voltage_Source u ->
            equals [ (V p, 1), (V q, -1)] [ ( Unit, realToFrac u ) ]
          Voltage_Source_Input ->
            equals [ (V p, 1), (V q, -1)] [ ( Unit, realToFrac inp) ]
          Opamp {} -> 
            equals [ (V p, 1), (V q, -1)] 
                   [ (V $ pos comp, opamp_gain), (V $ neg comp , negate opamp_gain) ]
      for_nodes = map Zero $ M.elems $ M.fromListWith plus $ do
        (e, (p, _, q)) <- edges c
        [ (p, singleton (C e) 1) , (q, singleton (C e) (-1)) ]
  in  [ equals [ (V $ ground c, 1) ] [] ] <> for_edges <> for_nodes
