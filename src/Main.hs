module Main where

import DebuggerImpl (defaultRunGhc, startCommandLine, execCommands, printAllBreaksInfo)

import ForeignDebugLib

main :: IO ()
main = defaultRunGhc startCommandLine


-- | tests below assume that TestMainModule.hs has the following code:
--module Main where
--
--qsort :: Ord a => [a] -> [a]
--qsort [] = []
--qsort (a:as) = qsort left ++ [a] ++ qsort right
--  where (left,right) = (filter (<=a) as, filter (>a) as)
--
--main :: IO ()
--main = print (qsort [8, 4, 0])
-- | it is not good to test in such way, but it is temporary decision

-- | test setting breaks
--main = defaultRunGhc $ printAllBreaksInfo "Main" >> execCommands [":break Main 4",
--                                                                 ":break Main 5",
--                                                                 ":trace main",
--                                                                 ":continue",
--                                                                 ":continue",
--                                                                 ":continue",
--                                                                 ":continue",
--                                                                 ":continue",
--                                                                 ":continue",
--                                                                 ":continue"]
-- | test delete breaks
--main = defaultRunGhc $ printAllBreaksInfo "Main" >> execCommands [":break Main 4",
--                                                                 ":break Main 5",
--                                                                 ":delete Main 2",
--                                                                 ":delete Main 9",
--                                                                 ":trace main"]
-- | test step into
--main = defaultRunGhc $ printAllBreaksInfo "Main" >> execCommands [":break Main 16",
--                                                                 ":trace main",
--                                                                 ":step",
--                                                                 ":step",
--                                                                 ":step",
--                                                                 ":step",
--                                                                 ":step",
--                                                                 ":continue"]
-- | test step over
--main = defaultRunGhc $ printAllBreaksInfo "Main" >> execCommands [":break Main 16",
--                                                                 ":trace main",
--                                                                 ":steplocal",
--                                                                 ":steplocal"]
-- | test input for next one
--module Main where
--
--qsort :: Ord a => [a] -> [a]
--qsort [] = []
--qsort (a:as) = qsort left ++ [a] ++ qsort right
--  where (left,right) = (filter (<=a) as, filter (>a) as)
--
--main :: IO ()
--main = print (qsort [8])
-- |
--main = defaultRunGhc $ printAllBreaksInfo "Main" >> execCommands [":break Main 5",
--                                                                 ":trace main",
--                                                                 ":steplocal",
--                                                                 ":steplocal",
--                                                                 ":steplocal",
--                                                                 ":steplocal",
--                                                                 ":steplocal",
--                                                                 ":steplocal",
--                                                                 ":steplocal",
--                                                                 ":steplocal",
--                                                                 ":steplocal",
--                                                                 ":steplocal",
--                                                                 ":steplocal"]
-- | test input for next one
--module Main where
--
--
--
--
--
--
--left x xs = filter (<=x) xs
--right x xs = filter (>x) xs
--
--qsort [] = []
--qsort (a:as) = qsort (left a as) ++ [a] ++ qsort (right a as)
--
--main :: IO ()
--main = print (qsort [8])
-- |
--main = defaultRunGhc $ printAllBreaksInfo "Main" >> execCommands [":break Main 12",
--                                                                 ":trace main",
--                                                                 ":steplocal",
--                                                                 ":steplocal",
--                                                                 ":steplocal",
--                                                                 ":steplocal",
--                                                                 ":steplocal",
--                                                                 ":steplocal",
--                                                                 ":steplocal",
--                                                                 ":steplocal",
--                                                                 ":steplocal",
--                                                                 ":steplocal"]