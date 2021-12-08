import Prelude hiding (Word)
import Data.List
import Input

type Word = [Int]

com :: Int -> [Word] -> Int;
com i ws = let { (m, n) = bp i ws } in
             if m > n; then 0; else 1

bp :: Int -> [Word] -> (Int, Int);
bp i = foldl' (\(m, n) w -> case w !! i of {
                              0 -> (m + 1, n);
                              1 -> (m, n + 1) })
              (0, 0)

oxygen_rating :: [Word] -> Word;
oxygen_rating = unfoldr gen_and_filter

gen_and_filter :: [Word] -> Maybe (Int, [Word]);
gen_and_filter ([]:_) = Nothing;
gen_and_filter ws     = let { b = com 0 ws } in
                          Just (b, reduce b ws)

reduce :: Int -> [Word] -> [Word];
reduce b = fmap tail . filter (\(c:_) -> b == c)

apo :: (b -> Maybe (a, Either b [a])) -> b -> [a];
apo f b = case f b of {
            Nothing            -> [];
            Just (x, Left v)   -> x : apo f v;
            Just (x, Right xs) -> x : xs }

-- See notes for why this is an apo-, rather than an unfold.
scrubber_rating :: [Word] -> Word;
scrubber_rating = apo gen_scrubber

bit_flip :: Int -> Int;
bit_flip 0 = 1;
bit_flip 1 = 0

gen_scrubber :: [Word] -> Maybe (Int, Either [Word] Word);
gen_scrubber ([] : _) = Nothing;
gen_scrubber ((b : bs) : []) = Just (b, Right bs);
gen_scrubber ws              =
  let { b = bit_flip (com 0 ws) } in
    Just (b, Left (reduce b ws))

be_val :: [Int] -> Int;
be_val = foldl (\d b -> (d * 2) + b) 0
