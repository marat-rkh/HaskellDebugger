module CmdArgsParser where

import ParserMonad

data Argument
    = SetPort Int
    | Unknown String

cmdArgument :: Parser Argument
setPortArgument :: Parser Argument
unknownArgument :: Parser Argument

cmdArgument = unlist [
        cmdArgument,
        unknownArgument
    ]

setPortArgument = do
    string "-p"
    p <- int
    end
    return $ SetPort p

unknownArgument = do
    arg <- restOfInput
    return $ Unknown arg
