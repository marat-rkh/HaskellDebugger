{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CmdArgsParser where

import Data.Char
import ParserMonad

data Argument
    = SetPort Int
    | Main String
    | Import String
    | ExposePkg String
    | Unknown String
        deriving Show

cmdArgument       :: Parser Argument
mainArgument      :: Parser Argument  -- -m<file>
importArgument    :: Parser Argument  -- -i<path>
setPortArgument   :: Parser Argument  -- -p<port>
exposePkgArgument :: Parser Argument  -- -pkg<package>
unknownArgument   :: Parser Argument

cmdArgument = unlist [
        mainArgument,
        importArgument,
        setPortArgument,
        exposePkgArgument,
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

exposePkgArgument = do
    string "-pkg"
    package <- restSatisfiedChars (not . isSpace)
    end
    return $ ExposePkg package

unknownArgument = do
    arg <- restOfInput
    return $ Unknown arg
