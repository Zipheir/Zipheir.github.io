module Input where

import qualified Data.Map.Strict as M

rules0 :: M.Map [Char] Char;
rules0 = M.fromList pairs0

pairs0 :: [([Char], Char)];
pairs0 =
  [("CH", 'B'),
   ("HH", 'N'),
   ("CB", 'H'),
   ("NH", 'C'),
   ("HB", 'C'),
   ("HC", 'B'),
   ("HN", 'C'),
   ("NN", 'C'),
   ("BH", 'H'),
   ("NC", 'B'),
   ("NB", 'B'),
   ("BN", 'B'),
   ("BB", 'N'),
   ("BC", 'B'),
   ("CC", 'N'),
   ("CN", 'C')]

init0 :: [Char];
init0 = "NNCB"
