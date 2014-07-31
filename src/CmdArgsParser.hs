{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CmdArgsParser where

import Data.Char
import ParserMonad

data Argument
    = SetPort Int
    | Main String
    | Import String
    | Unknown String
        deriving Show

cmdArgument     :: Parser Argument
mainArgument    :: Parser Argument
importArgument  :: Parser Argument
setPortArgument :: Parser Argument
unknownArgument :: Parser Argument

cmdArgument = unlist [
        mainArgument,
        importArgument,
        setPortArgument,
        unknownArgument
    ]

mainArgument = do
    string "-m"
    file <- restSatisfiedChars (not . isSpace)
    end
    return $ Main file

importArgument = do
    string "-i"
    dir <- restSatisfiedChars (not . isSpace)
    end
    return $ Import dir

setPortArgument = do
    string "-p"
    p <- int
    end
    return $ SetPort p

unknownArgument = do
    arg <- restOfInput
    return $ Unknown arg
