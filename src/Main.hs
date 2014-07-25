module Main where

import Debugger (defaultRunGhc, startCommandLine, execCommands, printAllBreaksInfo)

main :: IO ()
main = defaultRunGhc startCommandLine
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