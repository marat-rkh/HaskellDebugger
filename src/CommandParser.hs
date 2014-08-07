{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CommandParser where

import Data.Char
import ParserMonad

------------------------------------------------------------------------
data DebugCommand 
    = SetBreakpoint String Int    -- module, line
    | SetBreakByIndex String Int  -- module, index
    | RemoveBreakpoint String Int -- module, index
    | Resume
    | History
    | Back
    | Forward
    | StepInto
    | StepOver
    | Trace String                -- command
    | BreakList String            -- module
    | LineBreakList String Int    -- module line
    | Exit
    | Help
    | Print String                -- name of binding
    | SPrint String               -- name of binding
    | Force String                -- expr
    | ExprType String             -- expr
    | Evaluate Bool String        -- force, expr
    | Set String                  -- flag
    | Unset String                -- flag
    | Unknown
        deriving Show

debugCommand :: Parser DebugCommand
setBreakpoint :: Parser DebugCommand
setBreakByIndex :: Parser DebugCommand
removeBreakpoint :: Parser DebugCommand
resume :: Parser DebugCommand
history :: Parser DebugCommand
back :: Parser DebugCommand
forward :: Parser DebugCommand
stepInto :: Parser DebugCommand
stepOver :: Parser DebugCommand
trace :: Parser DebugCommand
breaklist :: Parser DebugCommand
lineBreaklist :: Parser DebugCommand
exit :: Parser DebugCommand
help :: Parser DebugCommand
printName :: Parser DebugCommand
sprintName :: Parser DebugCommand
force :: Parser DebugCommand
exprType :: Parser DebugCommand
evaluate :: Parser DebugCommand
set :: Parser DebugCommand
unset :: Parser DebugCommand
unknown :: Parser DebugCommand

debugCommand = unlist [
                        setBreakpoint,
                        setBreakByIndex,
                        removeBreakpoint,
                        resume,
                        history,
                        back,
                        forward,
                        stepInto,
                        stepOver,
                        trace,
                        breaklist,
                        lineBreaklist,
                        exit,
                        help,
                        printName,
                        sprintName,
                        force,
                        exprType,
                        evaluate,
                        set,
                        unset,
                        unknown
                      ]

setBreakpoint = do
    string ":break"
    waitAndSkipSpaces
    mod' <- restSatisfiedChars (not . isSpace)
    waitAndSkipSpaces
    line <- int
    skipSpaces
    end
    return $ SetBreakpoint mod' line

setBreakByIndex = do
    string ":breakindex"
    waitAndSkipSpaces
    moduleName <- restSatisfiedChars (not . isSpace)
    waitAndSkipSpaces
    ind <- int
    skipSpaces
    end
    return $ SetBreakByIndex moduleName ind

removeBreakpoint = do
    string ":delete"
    waitAndSkipSpaces
    moduleName <- restSatisfiedChars (not . isSpace)
    waitAndSkipSpaces
    ind <- int
    skipSpaces
    end
    return $ RemoveBreakpoint moduleName ind

resume = do
    string ":continue"
    skipSpaces
    end
    return Resume

history = do
    string ":history"
    skipSpaces
    end
    return History

back = do
    string ":back"
    skipSpaces
    end
    return Back

forward = do
    string ":forward"
    skipSpaces
    end
    return Forward

stepInto = do
    string ":step"
    skipSpaces
    end
    return StepInto

stepOver = do
    string ":steplocal"
    skipSpaces
    end
    return StepOver

trace = do
    string ":trace"
    waitAndSkipSpaces
    cmd <- restOfInput
    return $ Trace cmd

breaklist = do
    string ":breaklist"
    waitAndSkipSpaces
    mod' <- restSatisfiedChars (not . isSpace)
    skipSpaces
    end
    return $ BreakList mod'

lineBreaklist = do
    string ":breaklist"
    waitAndSkipSpaces
    mod' <- restSatisfiedChars (not . isSpace)
    waitAndSkipSpaces
    line <- int
    skipSpaces
    end
    return $ LineBreakList mod' line

exit = do
    string ":q"
    skipSpaces
    end
    return Exit

help = do
    string ":?"
    skipSpaces
    end
    return Help

printName = do
    string ":print"
    waitAndSkipSpaces
    name <- restSatisfiedChars (not . isSpace)
    skipSpaces
    end
    return $ Print name

sprintName = do
    string ":sprint"
    waitAndSkipSpaces
    name <- restSatisfiedChars (not . isSpace)
    skipSpaces
    end
    return $ SPrint name

force = do
    string ":force"
    waitAndSkipSpaces
    expr <- restOfInput
    return $ Force expr

exprType = do
    string ":type"
    waitAndSkipSpaces
    cmd <- restOfInput
    return $ ExprType cmd

evaluate = do
    string ":eval"
    waitAndSkipSpaces
    force_int <- int
    waitAndSkipSpaces
    cmd <- restOfInput
    return $ Evaluate (force_int > 0) cmd

set = do
    string ":set"
    waitAndSkipSpaces
    flag <- restSatisfiedChars (not . isSpace)
    skipSpaces
    end
    return $ Set flag

unset = do
    string ":unset"
    waitAndSkipSpaces
    flag <- restSatisfiedChars (not . isSpace)
    skipSpaces
    end
    return $ Unset flag

unknown = do
    restOfInput
    return $ Unknown
