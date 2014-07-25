module Main where

--qsort :: Ord a => [a] -> [a]
--qsort [] = []
--qsort (a:as) = qsort left ++ [a] ++ qsort right
--  where (left,right) = (filter (<=a) as, filter (>a) as)

left x xs = filter (<=x) xs
right x xs = filter (>x) xs

qsort [] = []
qsort (a:as) = qsort (left a as) ++ [a] ++ qsort (right a as)

main :: IO ()
main = print (qsort [8])
--main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])