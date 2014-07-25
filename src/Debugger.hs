{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Debugger where

import GHC hiding (resume)
import qualified GHC (resume)
import GHC.Paths ( libdir )
import DynFlags
import GhcMonad (liftIO)
import Outputable (Outputable, (<+>))
import qualified Outputable

import CommandParser (parse, debugCommand, DebugCommand(..))
import DebuggerMonad

import System.IO
import Exception (throwIO)
import Data.Maybe (isNothing, listToMaybe)
import Control.Monad (mplus)
import Data.List (partition, sortBy)
import BreakArray
import Data.Array
import Data.Function (on)

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
getCommand handle_ = do
    line <- liftIO $ hGetLine handle_
    return $ fst $ head $ parse debugCommand line

-- |Runs DebugCommand and returns True iff debug is finished
runCommand :: DebugCommand -> Debugger Bool
runCommand (SetBreakpoint modName line)   = setBreakpointLikeGHCiDo modName line >> return False
runCommand (RemoveBreakpoint modName ind) = deleteBreakpoint modName ind >> return False
runCommand (Trace command)                = doTrace command >> return False
runCommand Resume                         = doResume >> return False
runCommand StepInto                       = doStepInto >> return False
runCommand History                        = showHistory defaultHistSize True >> return False
runCommand Exit                           = return True
runCommand _                              = Debugger.back >> return False --printString debugOutput "# Unknown command" >> return False

-- | setBreakpoint version with selector just taking first of avaliable breakpoints
setBreakpointFirstOfAvailable :: String -> Int -> Debugger ()
setBreakpointFirstOfAvailable modName line = setBreakpoint modName line (Just . head)

-- | setBreakpoint version with selector choosing
-- |   - the leftmost complete subexpression on the specified line, or
-- |   - the leftmost subexpression starting on the specified line, or
-- |   - the rightmost subexpression enclosing the specified line
-- | This strategy is the same to one GHCi currently uses
setBreakpointLikeGHCiDo :: String -> Int -> Debugger ()
setBreakpointLikeGHCiDo modName line = setBreakpoint modName line selectOneOf
    where selectOneOf :: [(BreakIndex, SrcSpan)] -> Maybe (BreakIndex, SrcSpan)
          selectOneOf breaks_ = listToMaybe (sortBy (leftmost_largest `on` snd)  onelineBreaks) `mplus`
                                listToMaybe (sortBy (leftmost_smallest `on` snd) multilineBreaks) `mplus`
                                listToMaybe (sortBy (rightmost `on` snd) breaks_)
            where (onelineBreaks, multilineBreaks) = partition endEqLine breaksWithStartEqLine
                  endEqLine (_, RealSrcSpan r) = GHC.srcSpanEndLine r == line
                  endEqLine _ = False
                  breaksWithStartEqLine = [ br | br@(_, srcSpan) <- breaks_,
                                            case srcSpan of (RealSrcSpan r) -> GHC.srcSpanStartLine r == line; _ -> False ]

-- | finds all avaliable breakpoint for given line, then using selector takes one of them and activates it
-- | if no breaks are avaliable proper message is shown
setBreakpoint :: String -> Int -> ([(BreakIndex, SrcSpan)] -> Maybe (BreakIndex, SrcSpan)) -> Debugger ()
setBreakpoint modName line selector = do
    breaksForLine <- findBreaksForLine modName line
    case breaksForLine of 
        []           -> printString "# Breakpoints are not allowed for this line"
        bbs@(b : bs) -> do
            let mbBreakToSet = if (null bs) then Just b else selector bbs
            case mbBreakToSet of
                Just (breakIndex, srcSpan) -> do
                    res <- changeBreakFlagInModBreaks modName breakIndex True
                    let msg = if res then "# Breakpoint (index = "++ show breakIndex ++") was set here:\n" ++ show srcSpan
                              else "# Breakpoint was not set: setBreakOn returned False"
                    printString msg
                Nothing -> printString "# Breakpoint was not set: selector returned Nothing"

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

-- | modBreaks_flags of ModBreaks contains info about set and unset breakpoints for specified module
-- | This function just set or unset given flag
changeBreakFlagInModBreaks :: String -> Int -> Bool -> Debugger Bool
changeBreakFlagInModBreaks modName flagIndex flagValue = do
    modBreaks <- getModBreaks modName
    let breaksFlags = modBreaks_flags modBreaks
    liftIO $ setBreakFlag breaksFlags flagIndex flagValue

setBreakFlag :: BreakArray -> Int -> Bool -> IO Bool
setBreakFlag bArr ind flag | flag      = setBreakOn bArr ind
                           | otherwise = setBreakOff bArr ind

-- | ':delete <module name> <break index>' command
deleteBreakpoint ::  String -> Int -> Debugger ()
deleteBreakpoint modName breakIndex = do
    res <- changeBreakFlagInModBreaks modName breakIndex False
    let msg = if res then "# Breakpoint (index = "++ show breakIndex ++") was removed"
              else "# Breakpoint was not removed: incorrect index"
    printString msg

-- | ':trace' command
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
            printString "# Trace finished"
            printString "# Observable names:"
            printListOfOutputable names
            return True
        GHC.RunBreak _ names mbBreakInfo
            | isNothing  mbBreakInfo || canLogSpan (GHC.resumeSpan $ head resumes) -> do
                printString "# Stopped at"
                printOutputable (GHC.resumeSpan $ head resumes)
                printString "# Observable names:"
                printListOfOutputable names
                return False
            | otherwise -> do
                runResult <- GHC.resume canLogSpan GHC.SingleStep
                afterRunStmt canLogSpan runResult
        _ -> return False

-- | ':continue' command
doResume :: Debugger ()
doResume = doContinue (const True) GHC.RunToCompletion

-- |':step [<expr>]' command - general step command
doStepGeneral :: String -> Debugger ()
doStepGeneral []   = doContinue (const True) GHC.SingleStep
doStepGeneral expr = printString "':step <expr>' is not implemented yet"

-- |':step' command
doStepInto :: Debugger ()
doStepInto = doStepGeneral []

-- | ':history' command
showHistory :: Int -> Bool -> Debugger ()
showHistory num showVars = do
    resumes <- GHC.getResumeContext
    case resumes of
        [] -> printString "# Not stopped at breakpoint"
        (r:_) -> do
            printString "# Current history:"
            let hist = resumeHistory r
                (took, rest) = splitAt num hist
            spans <- mapM GHC.getHistorySpan took
            let idx = map (0-) [(1::Int) ..]
            mapM (\(i, s, h) -> printSDoc $ Outputable.int i <+>
                        -- todo: get full path of the file
                        Outputable.text ":" <+>
                        (Outputable.text . head . GHC.historyEnclosingDecls) h <+>
                        (Outputable.parens . Outputable.ppr) s
                ) (zip3 idx spans hist)
            printString $ if (null rest) then "# End of history" else "# End of visible history"

-- | ':back' command
back = do
    (names, i, pan) <- GHC.back
    printString "# Available names"
    printListOfOutputable names
    printString "# Some int"
    printSDoc $ Outputable.int i
    printOutputable pan

---- || Hardcoded parameters (temporary for testing) || -----------------

-- |Test module file
mainModulePath :: String
mainModulePath = "TestMainModule.hs"

-- |Test module name
mainModuleName :: String
mainModuleName = "Main"

-- |Context for test file
setupStandardContext :: Debugger ()
setupStandardContext = setupContext mainModulePath mainModuleName

-- |Default history size
defaultHistSize :: Int
defaultHistSize = 20


---- || Utils || -------------------------------------------------------

-- | Prints SDoc to a given stream
printSDoc :: Outputable.SDoc -> Debugger ()
printSDoc message = do
    st <- getDebugState
    dflags <- getDynFlags
    unqual <- getPrintUnqual
    liftIO $ Outputable.printForUser dflags (debugOutput st) unqual message
    return ()

-- | Prints Outputable to a given stream
printOutputable :: (Outputable d) => d -> Debugger ()
printOutputable message = printSDoc $ Outputable.ppr message

printListOfOutputable :: (Outputable d) => [d] -> Debugger ()
printListOfOutputable = mapM_ printOutputable

-- | Prints String to a given stream
printString :: String -> Debugger ()
printString message = printSDoc $ Outputable.text message

-- | Shows info from ModBreaks for given moduleName. It is useful for debuging of our debuger = )
printAllBreaksInfo :: String -> Debugger ()
printAllBreaksInfo modName = do
    modBreaks <- getModBreaks modName
    -- modBreaks_flags - 0 if unset 1 if set
    printString "# modBreaks_flags:"
    liftIO $ showBreakArray $ modBreaks_flags $ modBreaks
    -- modBreaks_locs - breakpoint index and SrcSpan
    printString "# modBreaks_locs:"
    mapM (\(i, e) -> printString (show i ++ " : " ++ show e)) $ assocs $ modBreaks_locs $ modBreaks
    -- modBreaks_decls - An array giving the names of the declarations enclosing each breakpoint
    printString "# modBreaks_decls:"
    mapM (\(i, ds) -> printString (show i ++ " : " ++ show ds)) $ assocs $ modBreaks_decls $ modBreaks
    return ()

-- | parse and execute commands in list (to auto tests)
-- | exapmle: main = defaultRunGhc $ execCommands [":trace main", ":q"] - load default module, make trace and exit
execCommands :: [String] -> Debugger ()
execCommands []       = return ()
execCommands (c : cs) = do
    runCommand $ fst $ head $ parse debugCommand c
    execCommands cs