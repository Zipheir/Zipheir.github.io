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

low_points :: Array (Int, Int) Int -> [(Int, Int)];
low_points hm = let { (m, n) = snd (bounds hm) } in
                  [ (i, j) | i <- [0..m],
                             j <- [0..n],
                             low hm (i, j)]

-- Rose tree hylomorphism operator.
hyloR :: (a -> c -> d) -> ([d] -> c) -> (b -> a) -> (b -> [b]) -> b -> d;
hyloR f g h l x = f (h x) (hyloF f g h l x)

hyloF :: (a -> c -> d) -> ([d] -> c) -> (b -> a) -> (b -> [b]) -> b -> c;
hyloF f g h l = g . map (hyloR f g h l) . l

expand :: Array (Int, Int) Int -> (Int, Int) -> [(Int, Int)];
expand hm (i, j) =
  let { h = hm ! (i, j) } in
    [ (k, l) | (k, l) <- neighbor_ixs (snd (bounds hm)) (i, j),
               inb h (hm ! (k, l)) ]

inb :: Int -> Int -> Bool;
inb h k = k /= 9 && k > h

enum_basins :: Array (Int, Int) Int -> (Int, Int) -> [(Int, Int)];
enum_basins hm = hyloR (:) concat id (expand hm)

basin_sizes :: Array (Int, Int) Int -> [Int];
basin_sizes hm = map (length . nub . enum_basins hm)
                     (low_points hm)

solution :: Array (Int, Int) Int -> Int;
solution = product . take 3 . sortBy (flip compare) . basin_sizes

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
main = putStrLn (show (solution (heightmap input0)))
