module Main where

import Debugger (defaultRunGhc, startCommandLine, execCommands, printAllBreaksInfo)

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
--main = defaultRunGhc $ printAllBreaksInfo "Main" >> execCommands [":break Main 9",
--                                                                 ":trace main",
--                                                                 ":step",
--                                                                 ":step",
--                                                                 ":step",
--                                                                 ":step",
--                                                                 ":step",
--                                                                 ":continue"]