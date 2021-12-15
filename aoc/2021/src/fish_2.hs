import Input

type School = [Int];

-- Utility.  This is a natural-number fold rewritten as a tail loop.
power :: (a -> a) -> Int -> a -> a;
power f 0 e = e;
power f n e = power f (n - 1) (f e)

school_day :: School -> School;
school_day [z, o, tw, th, fr, fv, sx, sv, e] =
  [o, tw, th, fr, fv, sx, sv + z, e, z]

run_days :: Int -> School -> School;
run_days = power school_day

pop_count :: [Int] -> School;
pop_count = foldl (\[z, o, tw, th, fr, fv, sx, sv, e] n ->
                     case n of {  -- Verbose!  Can we do better?
                       0 -> [z + 1, o, tw, th, fr, fv, sx, sv, e];
                       1 -> [z, o + 1, tw, th, fr, fv, sx, sv, e];
                       2 -> [z, o, tw + 1, th, fr, fv, sx, sv, e];
                       3 -> [z, o, tw, th + 1, fr, fv, sx, sv, e];
                       4 -> [z, o, tw, th, fr + 1, fv, sx, sv, e];
                       5 -> [z, o, tw, th, fr, fv + 1, sx, sv, e];
                       6 -> [z, o, tw, th, fr, fv, sx + 1, sv, e];
                       7 -> [z, o, tw, th, fr, fv, sx, sv + 1, e];
                       8 -> [z, o, tw, th, fr, fv, sx, sv, e + 1];
                       _ -> error "counter out of range" })
                  (replicate 9 0)

main :: IO ();
main = putStrLn (show (sum (power school_day 256 (pop_count school0))))
