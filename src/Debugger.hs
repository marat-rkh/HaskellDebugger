{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Debugger where

import GHC hiding (resume)
import qualified GHC (resume)
import GHC.Paths ( libdir )
import DynFlags
import GhcMonad (liftIO)
import Outputable (Outputable)
import qualified Outputable

import CommandParser (parse, debugCommand, DebugCommand(..))
import DebuggerMonad

import System.IO
import Exception (throwIO)
import Data.Maybe (isNothing)
import BreakArray
import Data.Array

---- || Debugger runner || --------------------------------------------------------------------------

main :: IO ()
main = defaultRunGhc $ startCommandLine

-- |Runs Ghc program with default settings
defaultRunGhc :: Debugger a -> IO ()
defaultRunGhc program = defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $
    startDebugger (do
        setupStandardContext
        program
        return ()
    ) initState

-- |Setup needed flags before running debug program
setupContext :: String -> String -> Debugger ()
setupContext pathToModule nameOfModule = do
    dflags <- getSessionDynFlags
    setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                , ghcLink   = LinkInMemory
                                }
    setTargets =<< sequence [guessTarget pathToModule Nothing]
    GHC.load LoadAllTargets
    setContext [IIModule $ mkModuleName nameOfModule]

-- |In loop waits for commands and executes them
startCommandLine :: Debugger ()
startCommandLine = whileNot (do {command <- getCommand stdin; runCommand command })

whileNot :: Debugger Bool -> Debugger ()
whileNot p = do
    x <- p
    if not x then whileNot p else return ()

-- |Translates line from input stream into DebugCommand
getCommand :: Handle -> Debugger DebugCommand
getCommand handle = do
    line <- liftIO $ hGetLine handle
    return $ fst $ head $ parse debugCommand line

-- |Runs DebugCommand and returns True iff debug is finished
runCommand :: DebugCommand -> Debugger Bool
runCommand (SetBreakpoint modName line) = setBreakpointFirstOfAvailable modName line >> return False
runCommand (Trace command)                 = doTrace command >> return False
runCommand Resume                          = doResume >> return False
--runCommand StepInto                 = doStepInto >>= handleRunResult
runCommand Exit                            = return True
runCommand _                               = printString debugOutput "# Unknown command" >> return False

-- | setBreakpoint version with selector just taking first of avaliable breakpoints
setBreakpointFirstOfAvailable :: String -> Int -> Debugger ()
setBreakpointFirstOfAvailable modName line = setBreakpoint modName line head

-- | setBreakpoint version with selector taking breakpoint which covers the biggest part of source code
-- | (not completed yet)
--setBreakpointWithBiggestSpan :: (GhcMonad m) => String -> Int -> m ()
--setBreakpointWithBiggestSpan modName line = setBreakpoint modName line $ selectWithBiggestSpan
--    where 
--        selectWithBiggestSpan :: [(BreakIndex, SrcSpan)] -> (BreakIndex, SrcSpan)
--        selectWithBiggestSpan breaks | not $ null multilineBreaks = maximumBy comp multilineBreaks
--                                     | otherwise                  = maximumBy comp breaks
--                                     where multilineBreaks = filter (\(_, srcSpan) -> not $ isOneLineSpan srcSpan) breaks
--                                           comp :: (BreakIndex, SrcSpan) -> (BreakIndex, SrcSpan) -> Ordering
--                                           comp (_, UnhelpfulSpan _) (_, UnhelpfulSpan _) = EQ
--                                           comp (_, UnhelpfulSpan _) (_, RealSrcSpan _)   = LT
--                                           comp (_, RealSrcSpan _) (_, UnhelpfulSpan _)   = GT
--                                           comp (_, RealSrcSpan rs1) (_, RealSrcSpan rs2) = 
--                                                if (linesWidthRs1 < linesWidthRs2) 
--                                                then LT
--                                                else if(linesWidthRs1 == linesWidthRs2 && colWidthRs1 == colWidthRs2)

-- | finds all avaliable breakpoint for given line, then using selector takes one of them and activates it
-- | if no breaks are avaliable proper message is shown
setBreakpoint :: String -> Int -> ([(BreakIndex, SrcSpan)] -> (BreakIndex, SrcSpan)) -> Debugger ()
setBreakpoint modName line selector = do
    breaksForLine <- findBreaksForLine modName line
    case breaksForLine of 
        []     ->   printString debugOutput "Breakpoints are not allowed for this line"
        b : bs -> do
                    let (breakToSetIndex, breakToSetSrcSpan) = if (null bs) then b else selector (b : bs)
                    modBreaks <- getModBreaks modName
                    let breaksFlags = modBreaks_flags modBreaks
                    res <- liftIO $ setBreakOn breaksFlags breakToSetIndex
                    printString debugOutput $ if res then "# Breakpoint was set here:\n" ++ show breakToSetSrcSpan else "# Breakpoint was not set"

-- | returns list of (BreakIndex, SrcSpan) for moduleName, where each element satisfies: BreakIndex == line
findBreaksForLine :: String -> Int -> Debugger [(BreakIndex, SrcSpan)]
findBreaksForLine modName line = filterBreaksLocations modName predLineEq
    where predLineEq = \(_, srcSpan) -> case srcSpan of UnhelpfulSpan _ -> False
                                                        RealSrcSpan r   -> line == (srcSpanStartLine r)

-- | returns list of (BreakIndex, SrcSpan) for moduleName, where each element satisfies predicate
filterBreaksLocations :: String -> ((BreakIndex, SrcSpan) -> Bool) -> Debugger [(BreakIndex, SrcSpan)]
filterBreaksLocations modName predicate = do
    modBreaks <- getModBreaks modName
    let breaksLocations = assocs $ modBreaks_locs modBreaks
    return $ filter predicate breaksLocations

-- | get ModBreaks for given modulename
getModBreaks :: String -> Debugger ModBreaks
getModBreaks modName = do
    module_ <- GHC.lookupModule (mkModuleName modName) Nothing
    Just modInfo <- getModuleInfo module_
    return $ modInfoModBreaks modInfo

---- | ':trace' command
doTrace :: String -> Debugger ()
doTrace []   = doContinue (const True) GHC.RunAndLogSteps
doTrace expr = do
    runResult <- GHC.runStmt expr GHC.RunAndLogSteps
    afterRunStmt (const True) runResult
    return ()

doContinue :: (SrcSpan -> Bool) -> SingleStep -> Debugger ()
doContinue canLogSpan step = do
    runResult <- GHC.resume canLogSpan step
    afterRunStmt canLogSpan runResult
    return ()

afterRunStmt :: (SrcSpan -> Bool) -> GHC.RunResult -> Debugger Bool
afterRunStmt _ (GHC.RunException e) = liftIO $ throwIO e
afterRunStmt canLogSpan runResult = do
    resumes <- GHC.getResumeContext
    case runResult of
        GHC.RunOk names -> do
            printString debugOutput "# Trace finished"
            printString debugOutput "# Observable names:"
            printListOfOutputable debugOutput names
            return True
        GHC.RunBreak _ names mbBreakInfo
            | isNothing  mbBreakInfo || canLogSpan (GHC.resumeSpan $ head resumes) -> do
                printString debugOutput "# Stopped at"
                printOutputable debugOutput (GHC.resumeSpan $ head resumes)
                printString debugOutput "# Observable names:"
                printListOfOutputable debugOutput names
                return False
            | otherwise -> do
                runReuslt <- GHC.resume canLogSpan GHC.SingleStep
                afterRunStmt canLogSpan runReuslt
        _ -> return False

doResume :: Debugger()
doResume = doContinue (const True) GHC.RunToCompletion

---- |:step [<expr>] command - genegal step command
--doStepGeneral :: (GhcMonad m) => String -> m RunResult
--doStepGeneral []   = GHC.resume (const True) GHC.SingleStep
--doStepGeneral expr = printString debugOutput "':step <expr>' is not implemented yet" >> return (RunOk [])
--
---- |performs ':step' command
--doStepInto :: (GhcMonad m) => m RunResult
--doStepInto = doStepGeneral []


---- || Hardcoded parameters (temporary for testing) || -----------------

debugOutput :: Handle
debugOutput = stdout

-- |Test module file
mainModulePath :: String
mainModulePath = "TestMainModule.hs"

-- |Test module name
mainModuleName :: String
mainModuleName = "Main"

-- |Context for test file
setupStandardContext :: Debugger ()
setupStandardContext = setupContext mainModulePath mainModuleName


---- || Utils || -------------------------------------------------------

-- |Prints SDoc to a given stream
printSDoc :: Handle -> Outputable.SDoc -> Debugger ()
printSDoc handle message = do
    dflags <- getDynFlags
    unqual <- getPrintUnqual
    liftIO $ Outputable.printForUser dflags handle unqual message
    return ()

-- |Prints Outputable to a given stream
printOutputable :: (Outputable d) => Handle -> d -> Debugger ()
printOutputable handle message = printSDoc handle $ Outputable.ppr message

printListOfOutputable :: (Outputable d) => Handle -> [d] -> Debugger ()
printListOfOutputable handle = mapM_ (printOutputable handle)

-- |Prints String to a given stream
printString :: Handle -> String -> Debugger ()
printString handle message = printSDoc handle $ Outputable.text message

-- | Shows info from ModBreaks for given moduleName. It is useful for debuging of our debuger = )
printAllBreaksInfo :: String -> Debugger ()
printAllBreaksInfo modName = do
    modBreaks <- getModBreaks modName
    -- modBreaks_flags - 0 if unset 1 if set
    printString debugOutput "# modBreaks_flags:"
    liftIO $ showBreakArray $ modBreaks_flags $ modBreaks
    -- modBreaks_locs - breakpoint index and SrcSpan
    printString debugOutput "# modBreaks_locs:"
    mapM (\(i, e) -> printString debugOutput (show i ++ " : " ++ show e)) $ assocs $ modBreaks_locs $ modBreaks
    -- modBreaks_decls - An array giving the names of the declarations enclosing each breakpoint
    printString debugOutput "# modBreaks_decls:"
    mapM (\(i, ds) -> printString debugOutput (show i ++ " : " ++ show ds)) $ assocs $ modBreaks_decls $ modBreaks
    return ()

-- | parse and execute commands in list (to auto tests)
-- | exapmle: main = defaultRunGhc $ execCommands [":trace main", ":q"] - load default module, make trace and exit
execCommands :: [String] -> Debugger ()
execCommands []       = return ()
execCommands (c : cs) = do
    runCommand $ fst $ head $ parse debugCommand c
    execCommands cs