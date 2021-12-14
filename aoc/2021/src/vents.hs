import Data.List
import qualified Data.Map.Strict as Map
import Input

type Seg = ((Int, Int), (Int, Int))

build_map :: [Seg] -> Map.Map (Int, Int) Int;
build_map segs = let { ps = concat (map (zip1 . points) segs) } in
                   Map.fromListWith (+) ps

zip1 :: [a] -> [(a, Int)];
zip1 = map (\x -> (x, 1))

is_vertical :: Seg -> Bool;
is_vertical ((x1, _), (x2, _)) = x1 == x2

is_horizontal :: Seg -> Bool;
is_horizontal ((_, y1), (_, y2)) = y1 == y2

points :: Seg -> [(Int, Int)];
points p =
  if is_vertical p;
     then points_v p;
     else if is_horizontal p;
             then points_h p;
             else points_d p

points_v :: Seg -> [(Int, Int)];
points_v ((x, y1), (_, y2)) = [(x, y) | y <- [min y1 y2 .. max y1 y2]]

points_h :: Seg -> [(Int, Int)];
points_h ((x1, y), (x2, _)) = [(x, y) | x <- [min x1 x2 .. max x1 x2]]

-- Part 2 only.
points_d :: Seg -> [(Int, Int)];
points_d ((x1, y1), (x2, y2)) =
  if x1 < x2;
     then
       if y1 < y2;
          then sym_inc ((x1, y1), (x2, y2));   -- 45°
          else dec_y ((x1, y1), (x2, y2));     -- 315°
     else
       if y1 < y2;
          then dec_x ((x1, y1), (x2, y2));   -- 135°
          else sym_inc ((x2, y2), (x1, y1))  -- 225°

sym_inc :: Seg -> [(Int, Int)];
sym_inc ((x1, y1), (x2, y2)) =
  zip [x | x <- [x1 .. x2]] [y | y <- [y1 .. y2]]

dec_x :: Seg -> [(Int, Int)];
dec_x ((x1, y1), (x2, y2)) =
  unfoldr (\(x, y) -> if x < x2 && y > y2;
                         then Nothing;
                         else Just ((x, y), (x - 1, y + 1)))
          (x1, y1)

dec_y :: Seg -> [(Int, Int)];
dec_y ((x1, y1), (x2, y2)) =
  unfoldr (\(x, y) -> if x > x2 && y < y2;
                         then Nothing;
                         else Just ((x, y), (x + 1, y - 1)))
          (x1, y1)

-- Utility function
count :: Foldable t => (a -> Bool) -> t a -> Integer;
count p = foldl' (\c x -> if p x; then c + 1; else c) 0

overlaps :: [Seg] -> Integer;
overlaps = count (> 1) . build_map

-- Part 1 only.
v_or_h :: [Seg] -> [Seg];
v_or_h = filter (\s -> is_vertical s || is_horizontal s)

main :: IO ();
main = putStrLn (show (overlaps segments1))
