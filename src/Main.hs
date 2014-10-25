module Main where

import DebuggerImpl (defaultRunGhc, startCommandLine, execCommands, printAllBreaksInfo)

main :: IO ()
main = defaultRunGhc startCommandLine
