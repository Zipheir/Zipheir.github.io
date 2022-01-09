import Data.List ((\\))
import qualified Data.Map.Strict as M
import Control.Monad.State.Lazy
import qualified Data.Set as S

import Input

type Grid = M.Map (Int, Int) Int

steps :: Int -> Grid -> Int;
steps n g = snd ((iterate step (g, 0)) !! n)

step :: (Grid, Int) -> (Grid, Int);
step (g, c) = let { g' = resolve_flashes (inc_all g) } in
                (reset_flashed g', c + num_flashers g')

inc_all :: Grid -> Grid;
inc_all = fmap (+1)

reset_flashed :: Grid -> Grid;
reset_flashed = fmap (\n -> if n > 9 then 0 else n)

num_flashers :: Grid -> Int;
num_flashers = M.foldl' (\c e -> if e > 9; then c + 1; else c) 0

resolve_flashes :: Grid -> Grid;
resolve_flashes g = fst (execState (resolve (flashers g)) (g, S.empty))

flashers :: Grid -> [(Int, Int)];
flashers = M.keys . M.filter (> 9)

resolve :: [(Int, Int)] -> State (Grid, S.Set (Int, Int)) ()
resolve []       = return ();
resolve (c : cs) = do { new <- resolve1 c;
                        resolve (push_adj new cs) }

push_adj :: Eq a => [a] -> [a] -> [a];
push_adj xs ys = (xs \\ ys) ++ ys

resolve1 :: (Int, Int) -> State (Grid, S.Set (Int, Int)) [(Int, Int)];
resolve1 c =
  state (\(g, s) ->
           let { g' = flash g c;
                 ns = neighbors g' c;
                 cs = map fst (filter ((> 9) . snd) ns);
                 s' = S.insert c s }
             in (filter (\d -> not (S.member d s')) cs, (g', s')))

neighbors :: Grid -> (Int, Int) -> [((Int, Int), Int)];
neighbors g c = M.foldrWithKey (\p e ps ->
                                 if in_square c p;
                                    then (p, e) : ps;
                                    else ps)
                               []
                               g

in_square :: (Int, Int) -> (Int, Int) -> Bool;
in_square (i, j) (k, m) = k >= i - 1 && k <= i + 1 && m >= j - 1 &&
                          m <= j + 1

flash :: Grid -> (Int, Int) -> Grid;
flash g c = M.mapWithKey (\d e ->
                           if in_square c d && c /= d;
                              then e + 1;
                              else e)
                         g

make_grid :: [[Int]] -> Grid;
make_grid xss =
  let { r    = length xss - 1;
        c    = length (head xss) - 1;
        rows = zip [0..r] xss }
    in M.fromList
         (concatMap (\(r, xs) ->
                       map (\(c, x) -> ((r, c), x))
                           (zip [0..c] xs))
                    rows)

-- Temporary
chunks :: Int -> [a] -> [[a]];
chunks _ [] = [];
chunks n xs = take n xs : chunks n (drop n xs)

main :: IO ();
main = let { g = make_grid input1 } in
         putStrLn (show (steps 100 g))
