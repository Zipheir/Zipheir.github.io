module Input where

bitstrings0 :: [[Int]];
bitstrings0 = map (map c_to_b) input0

c_to_b :: Char -> Int;
c_to_b '0' = 0;
c_to_b '1' = 1

input0 :: [String];
input0 = 
  ["00100",
   "11110",
   "10110",
   "10111",
   "10101",
   "01111",
   "00111",
   "11100",
   "10000",
   "11001",
   "00010",
   "01010"]
