import Data.List
import Input

-- For 5x5 boards, lists work just fine.
type Board = [[(Int, Bool)]]

highest :: Int;
highest = 4

ref :: Board -> (Int, Int) -> (Int, Bool);
ref b (i, j) = (b !! i) !! j

-- Return a board with k marked.
mark :: Board -> Int -> Board;
mark b k = let { mark1 (x, mk) = if x == k;
                                    then (x, True);
                                    else (x, mk) } in
             fmap (fmap mark1) b

-- Is k marked in b?
marked :: Board -> Int -> Bool;
marked b k = any (any (\(x, mk) -> x == k && mk)) b

-- Is the position (x, y) marked in b?
cell_marked :: Board -> (Int, Int) -> Bool;
cell_marked b (x, y) = snd (ref b (x, y))

rows :: Board -> [[(Int, Bool)]];
rows = id

cols :: Board -> [[(Int, Bool)]];
cols = transpose

-- Is a row/column entirely marked?
rcmarked :: [(Int, Bool)] -> Bool;
rcmarked = all snd

-- Is a board in a winning state?
won :: Board -> Bool;
won b = any rcmarked (rows b) || any rcmarked (cols b)

-- Run a bingo game with a list of boards and a number stream, and
-- produce the first winning board (assumed to be unique).
bingo :: [Board] -> [Int] -> Int;
bingo _  []       = error "game ran out of numbers!";
bingo bs (n : ns) = let { bs' = marks n bs } in
                      case winner bs' of {
                        Nothing -> bingo bs' ns;
                        Just b  -> score b n }

-- Mark k in every board in a list.
marks :: Int -> [Board] -> [Board];
marks k = fmap (\b -> mark b k)

-- Find the winning board in a list, if it exists.
winner :: [Board] -> Maybe Board;
winner = find won

score :: Board -> Int -> Int;
score b n = n * sum_um b

-- Sum the unmarked elements of a board.
sum_um :: Board -> Int;
sum_um = foldr add_um 0 . concat

add_um :: (Int, Bool) -> Int -> Int;
add_um (_, True)  n = n;
add_um (x, False) n = x + n
