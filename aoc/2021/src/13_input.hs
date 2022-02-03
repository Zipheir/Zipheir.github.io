module Input where

import Data.List
import Data.Ord
import qualified Data.Set as S
import Data.Array

input_pairs0 :: [(Int, Int)];
input_pairs0 = [(6,10), (0,14), (9,10), (0,3), (10,4), (4,11), (6,0),
                (6,12), (4,1), (0,13), (10,12), (3,4), (3,0), (8,4),
                (1,10), (2,14), (8,10), (9,0)]

build_grid :: [(Int, Int)] -> Array (Int, Int) Bool;
build_grid ps =
  let { cmax = fst (maximumBy (comparing fst) ps);
        rmax = snd (maximumBy (comparing snd) ps);
        s    = S.fromList ps                       } in
    array ((0, 0), (cmax, rmax))
      [ ((c, r), S.member (c, r) s) | r <- [0..rmax], c <- [0..cmax] ]

grid0 :: Array (Int, Int) Bool;
grid0 = build_grid input_pairs0

data Inst = X Int | Y Int;

insts0 :: [Inst];
insts0 = [Y 7, X 5];
