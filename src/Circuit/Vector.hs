-- | vectors and systems of linear equations

{-# language GeneralizedNewtypeDeriving #-}

module Circuit.Vector where

import qualified Numeric.LinearAlgebra as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S

newtype Vector ix val = Vector (M.Map ix val)
  deriving (Show, Functor, Foldable)

sparse kvs = Vector $ M.fromListWith (+) kvs

singleton ix val = Vector $ M.singleton ix val

plus (Vector xs) (Vector ys) = Vector $ M.unionWith (+) xs ys

minus u v = plus u $ fmap negate v

assocs (Vector xs) = M.toList xs


data Equation var = Zero (Vector var Double)
  deriving Show

equals xs ys = Zero $ minus (sparse xs) (sparse ys)

solve :: Ord unk => unk -> [ Equation unk ] -> Vector unk Double
solve unit eqs =
  let unknowns = S.toList $ S.fromList $ do
        Zero v <- eqs
        map fst $ assocs v
      index = M.fromList $ zip unknowns [ 0 :: Int .. ]
      a :: L.Matrix Double
      a = L.assoc (1 + length eqs, length unknowns) 0 $ do
        (row, Zero v) <- zip [0:: Int ..] $ equals [(unit, 1)] [] : eqs
        (u, c) <- assocs v
        return ((row, index M.! u), c)
      v :: L.Vector Double
      v = L.assoc (1 + length eqs) 0 [(0, 1)]
      x = a L.<\> v
      get u = L.atIndex x $ index M.! u
  in  sparse $ do u <- unknowns ; return ( u, get u / get unit )

