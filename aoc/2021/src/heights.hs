import Data.List
import Data.Array
import Input

neighbors :: Array (Int, Int) Int -> (Int, Int) -> [Int];
neighbors hm = map (hm !) . neighbor_ixs (snd (bounds hm))

neighbor_ixs :: (Int, Int) -> (Int, Int) -> [(Int, Int)];
neighbor_ixs (m, n) (r, c) =
  filter (/= (r, c))
    (nub [(mon r 1, c), (r, mon c 1), (r, min (c + 1) n),
          (min (r + 1) m, c)])

mon :: Int -> Int -> Int;
mon a b = max 0 (a - b)

low :: Array (Int, Int) Int -> (Int, Int) -> Bool;
low hm (r, c) =
  let { e  = hm ! (r, c);
        ns = neighbors hm (r, c) } in
    all (\n -> e < n) ns

low_points :: Array (Int, Int) Int -> [Int];
low_points hm = let { (m, n) = snd (bounds hm) } in
                  [ hm ! (i, j) | i <- [0..m],
                                  j <- [0..n],
                                  low hm (i, j)]

risk :: Int -> Int;
risk = (+1)

-- Convert a list of lists to a height map.
heightmap :: [[a]] -> Array (Int, Int) a;
heightmap xss =
  let { r    = length xss - 1;
        c    = length (head xss) - 1;
        rows = zip [0..r] xss } in
    array ((0, 0), (r, c))
          (concatMap (\(r, xs) ->
                        map (\(c, x) -> ((r, c), x)) (zip [0..c] xs))
                     rows)

main :: IO ();
main = let { s = sum (map risk (low_points (heightmap input1))) } in
         putStrLn (show s)
