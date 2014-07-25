module Main where

import Debugger (defaultRunGhc, startCommandLine, execCommands, printAllBreaksInfo)

main :: IO ()
main = defaultRunGhc startCommandLine
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