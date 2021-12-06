import Input
import Data.Array

-- Implementation for 12-bit strings.

main :: IO ();
main = let { coms = cbs bitstrings } in
         putStrLn (show (gamma coms, epsilon coms))

zeros :: Array Int (Int, Int);
zeros = array (0, 11) [ (i, (0, 0)) | i <- [0..11]]

bp_all :: [[Int]] -> Array Int (Int, Int);
bp_all = foldr aap zeros

aap :: [Int] -> Array Int (Int, Int) -> Array Int (Int, Int);
aap bs v = array (0, 11) [ (i, add_pos i bs (v ! i)) | i <- [0..11] ]

add_pos :: Int -> [Int] -> (Int, Int) -> (Int, Int);
add_pos i bs (m, n) = if (bs !! i) == 0;
                         then (m + 1, n);
                         else (m, n + 1)

-- Compute the (most, least) common bit for each position in the list
-- of fixed-length bitstrings bss.
cbs :: [[Int]] -> Array Int Int;
cbs bss = let { com (m, n) = if m >= n; then 0; else 1 } in
            fmap com (bp_all bss)

-- The "gamma rate" is encoded (in big-endian binary) by the most
-- common bits in each input position.
gamma :: Array Int Int -> Int;
gamma = be_val . elems

-- The "epsilon rate" is encoded (in big-endian binary) by the least
-- common bits in each input position.
epsilon :: Array Int Int -> Int;
epsilon = be_val . map bit_flip . elems

be_val :: [Int] -> Int;
be_val = foldl (\d b -> (d * 2) + b) 0

bit_flip :: Int -> Int;
bit_flip 0 = 1;
bit_flip 1 = 0
