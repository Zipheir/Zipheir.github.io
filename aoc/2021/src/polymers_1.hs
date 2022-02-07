import Data.List
import qualified Data.Map.Strict as M
import Input

type Element = Char

pair_insert :: [Element] -> Element;
pair_insert = (rules0 M.!)

step :: [Element] -> [Element];
step []             = [];
step [e]            = [e];
step (e1 : e2 : es) = e1 : (pair_insert [e1, e2]) : step (e2 : es)

iter :: Int -> (a -> a) -> a -> a;
iter n f x
  | n == 0    = x
  | n > 0     = iter (n - 1) f (f x)
  | otherwise = error "negative count"

steps :: Int -> [Element] -> [Element];
steps n = iter n step

limits :: [Element] -> (Int, Int);
limits es = let { gs = map length (group (sort es)) } in
              (maximum gs, minimum gs)

solution :: [Element] -> Int;
solution es = let { (mx, mn) = limits (steps 10 es) } in
                mx - mn

main :: IO ();
main = putStrLn (show (solution init0))
