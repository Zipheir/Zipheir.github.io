import Data.Array
import Control.Exception (assert)
import Input
--import Data.Set as S
import qualified Data.Foldable as F

type Grid = Array (Int, Int) Bool

hbounds :: Grid -> (Int, Int);
hbounds = snd . bounds

bisects :: Integral a => a -> a -> Bool;
bisects s k = let { (q, r) = quotRem s 2 } in
                k == q && r == 0

crease_row :: Int -> Grid -> Grid;
crease_row k g =
  let { (cmax_g, rmax_g) = hbounds g;
        rmax             = assert (bisects rmax_g k) rmax_g - k - 1;
        is_set (i, j)    = g ! (i, j) || g ! (i, rmax_g - j) } in
    array ((0, 0), (cmax_g, rmax))
      [ ((i, j), is_set (i, j)) | i <- [0..cmax_g], j <- [0..rmax] ]

crease_col :: Int -> Grid -> Grid;
crease_col k g =
  let { (cg, rg)      = hbounds g;
        cmax          = assert (bisects cg k) k - 1;
        is_set (i, j) = g ! (cmax - i, j) || g ! (i + k + 1, j) } in
    array ((0, 0), (cmax, rg))
      [ ((i, j), is_set (i, j)) | i <- [0..cmax], j <- [0..rg] ]

marked :: Grid -> Int;
marked = count id

-- Utility
count :: Foldable t => (a -> Bool) -> t a -> Int;
count p = F.foldl' (\c x -> if p x; then c + 1; else c) 0

print_row :: Grid -> Int -> IO ();
print_row g r = sequence_
                  (map putStr
                    (map (\c -> if g ! (c, r) then "#" else ".")
                         [0 .. (fst (hbounds g))])
                    ++ [putChar '\n'])

print_grid :: Grid -> IO ();
print_grid g = sequence_ (map (print_row g) [0 .. (snd (hbounds g))])

creases :: Grid -> [Inst] -> Grid;
creases init = F.foldl' (\g c ->
                           case c of {
                             X k -> crease_col k g;
                             Y k -> crease_row k g })
                        init

solution1 :: Int;
solution1 = marked (creases grid1 (take 1 insts1))

-- Why is the final grid horizontally flipped?
solution2 :: IO ();
solution2 = print_grid (hflip (creases grid1 insts1))

hflip :: Grid -> Grid;
hflip g = let { (c, r) = hbounds g } in
            array ((0, 0), (c, r))
              [ ((i, j), g ! (c - i, j)) | i <- [0..c], j <- [0..r] ]

main :: IO ();
main = solution2
