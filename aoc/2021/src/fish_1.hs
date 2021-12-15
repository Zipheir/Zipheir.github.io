import Input

type Fish = Int;

-- Utility.  This is a natural-number fold rewritten as a tail loop.
power :: (a -> a) -> Int -> a -> a;
power f 0 e = e;
power f n e = power f (n - 1) (f e)

day :: Fish -> [Fish];
day 0 = [6, 8];
day n = if n > 0; then [n-1]; else error "negative counter!"

school_day :: [Fish] -> [Fish];
school_day = concat . map day

run_days :: Int -> [Fish] -> [Fish];
run_days = power school_day

main :: IO ();
main = putStrLn (show (length (run_days 80 school1)))
