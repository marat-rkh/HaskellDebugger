{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module DebuggerImpl where

import GHC hiding (resume)
import qualified GHC (resume)
import GHC.Paths ( libdir )
import DynFlags
import GhcMonad (liftIO)

import ParserMonad (parse)
import CommandParser (debugCommand, DebugCommand(..))
import CmdArgsParser (cmdArgument, Argument(..))
import DebuggerMonad

import Network.Socket
import Network.BSD

import System.IO
import System.Environment (getArgs)
import Control.Monad (mplus, liftM)
import Data.List (partition, sortBy)
import BreakArray
import Data.Array
import Data.Function (on)
import SrcLoc (realSrcSpanEnd)

import Control.Exception (SomeException, throwIO)

import Data.Maybe
import DebuggerUtils

import Debugger (pprintClosureCommand)

---- || Debugger runner || --------------------------------------------------------------------------

tryRun :: DebuggerMonad a -> DebuggerMonad (Maybe a)
tryRun func = do {
    result <- func;
    return $ Just result;
} `gcatch` (\ex -> do {
    printJSON [
            ("info", ConsStr "exception"),
            ("message", ConsStr $ show (ex::SomeException))
        ];
    return Nothing;
})


-- |Runs Ghc program with default settings
defaultRunGhc :: DebuggerMonad a -> IO ()
defaultRunGhc program = defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $
    startDebugger (do
        setupStandardContext
        handleArguments
        initDebugOutput
        program
        return ()
    ) initState

-- |Setup needed flags before running debug program
setupContext :: String -> String -> DebuggerMonad ()
setupContext pathToModule nameOfModule = do
    dflags <- getSessionDynFlags
    setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                , ghcLink   = LinkInMemory
                                , ghcMode   = CompManager
                                }
    setTargets =<< sequence [guessTarget pathToModule Nothing]
    GHC.load LoadAllTargets
    setContext [IIModule $ mkModuleName nameOfModule]

handleArguments :: DebuggerMonad ()
handleArguments = do
    args <- liftIO getArgs
    mapM_ handle' args where
        handle' x = do
            let arg = fst $ head $ parse cmdArgument x
            case arg of
                SetPort p -> do modifyDebugState $ \st -> st{port = Just p}
                CmdArgsParser.Unknown s -> printJSON [
                        ("info", ConsStr "warning"),
                        ("message", ConsStr $ "unknown argument: " ++ s)
                    ]

initDebugOutput :: DebuggerMonad ()
initDebugOutput = do
    st <- getDebugState
    let m_port = port st
    case m_port of
        Nothing -> printJSON [
                ("info", ConsStr "warning"),
                ("message", ConsStr "Port for debug stream was not set (using stdout); use command line argument -p<port> to set it")
            ]
        Just port' -> do
            let host = "localhost"
            sock <- liftIO $ socket AF_INET Stream 0
            addrs <- liftIO $ liftM hostAddresses $ getHostByName host
            do {
                liftIO $ connect sock $ SockAddrInet (toEnum port') (head addrs);
                handle <- liftIO $ socketToHandle sock ReadWriteMode;
                modifyDebugState $ \st' -> st'{debugOutput = handle};
                printJSON [
                        ("info", ConsStr "connected to port"),
                        ("port", ConsInt port')
                    ];
            } `gcatch` (\ex -> printJSON [
                        ("info", ConsStr "exception"),
                        ("message", ConsStr $ show (ex::SomeException) ++ "; using stdout for debug output")
                    ])

-- |In loop waits for commands and executes them
startCommandLine :: DebuggerMonad ()
startCommandLine = whileNot $ do
    command <- getCommand stdin
    m_result <- tryRun (runCommand command)
    case m_result of
        Just result -> return result
        Nothing -> return False

whileNot :: DebuggerMonad Bool -> DebuggerMonad ()
whileNot p = do
    x <- p
    if not x then whileNot p else return ()

-- |Translates line from input stream into DebugCommand
getCommand :: Handle -> DebuggerMonad DebugCommand
getCommand handle_ = do
    line <- liftIO $ hGetLine handle_
    return $ fst $ head $ parse debugCommand line

-- |Runs DebugCommand and returns True iff debug is finished
runCommand :: DebugCommand -> DebuggerMonad Bool
runCommand (SetBreakpoint modName line)   = setBreakpointLikeGHCiDo modName line >> return False
runCommand (RemoveBreakpoint modName ind) = deleteBreakpoint modName ind >> return False
runCommand (Trace command)                = doTrace command >> return False
runCommand Resume                         = doResume >> return False
runCommand StepInto                       = doStepInto >> return False
runCommand StepOver                       = doStepLocal >> return False
runCommand History                        = showHistory defaultHistSize >> return False
runCommand Exit                           = return True
runCommand (BreakList modName)            = showBreaks modName >> return False
runCommand Help                           = printString fullHelpText >> return False
runCommand (Print name)                   = doPrint name >> return False
runCommand (SPrint name)                  = doSPrint name >> return False
runCommand (Force expr)                   = doForce expr >> return False
runCommand _                              = printJSON [
                                                ("info", ConsStr "exception"),
                                                ("message", ConsStr "unknown command")
                                            ] >> return False

-- | setBreakpoint version with selector just taking first of avaliable breakpoints
setBreakpointFirstOfAvailable :: String -> Int -> DebuggerMonad ()
setBreakpointFirstOfAvailable modName line = setBreakpoint modName line (Just . head)

-- | setBreakpoint version with selector choosing
-- |   - the leftmost complete subexpression on the specified line, or
-- |   - the leftmost subexpression starting on the specified line, or
-- |   - the rightmost subexpression enclosing the specified line
-- | This strategy is the same to one GHCi currently uses
setBreakpointLikeGHCiDo :: String -> Int -> DebuggerMonad ()
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
setBreakpoint :: String -> Int -> ([(BreakIndex, SrcSpan)] -> Maybe (BreakIndex, SrcSpan)) -> DebuggerMonad ()
setBreakpoint modName line selector = do
    breaksForLine <- findBreaksForLine modName line
    case breaksForLine of 
        []           -> printJSON [
                                ("info", ConsStr "breakpoint was not set"),
                                ("add_info", ConsStr "not allowed here")
                            ]
        bbs@(b : bs) -> do
            let mbBreakToSet = if (null bs) then Just b else selector bbs
            case mbBreakToSet of
                Just (breakIndex, srcSpan) -> do
                    res <- changeBreakFlagInModBreaks modName breakIndex True
--                    let msg = if res then "# Breakpoint (index = "++ show breakIndex ++") was set here:\n" ++ show srcSpan
--                              else "# Breakpoint was not set: setBreakOn returned False"
                    if res
                        then printJSON [
                                ("info", ConsStr "breakpoint was set"),
                                ("index", ConsInt $ breakIndex),
                                ("src_span", srcSpanAsJSON srcSpan)
                            ]
                        else printJSON [
                                 ("info", ConsStr "breakpoint was not set"),
                                 ("add_info", ConsStr "selector returned Nothing")
                             ]
--                    printString msg
                Nothing -> printString "# Breakpoint was not set: selector returned Nothing"

-- | returns list of (BreakIndex, SrcSpan) for moduleName, where each element satisfies: srcSpanStartLine == line
-- | todo: this function is very suboptimal, it is temporary decition
findBreaksForLine :: String -> Int -> DebuggerMonad [(BreakIndex, SrcSpan)]
findBreaksForLine modName line = filterBreaksLocations modName predLineEq
    where predLineEq = \(_, srcSpan) -> case srcSpan of UnhelpfulSpan _ -> False
                                                        RealSrcSpan r   -> line == (srcSpanStartLine r)

-- | returns list of (BreakIndex, SrcSpan) for moduleName, where each element satisfies: srcSpanStartLine <= line <= srcSpanEndLine
-- | todo: this function is very suboptimal, it is temporary decition
findBreaksContainingLine :: String -> Int -> DebuggerMonad [(BreakIndex, SrcSpan)]
findBreaksContainingLine modName line = filterBreaksLocations modName predLineEq
    where predLineEq = \(_, srcSpan) -> case srcSpan of UnhelpfulSpan _ -> False
                                                        RealSrcSpan r   -> (srcSpanStartLine r) <= line && line <= (srcSpanEndLine r)

-- | returns list of (BreakIndex, SrcSpan) for moduleName, where each element satisfies predicate
filterBreaksLocations :: String -> ((BreakIndex, SrcSpan) -> Bool) -> DebuggerMonad [(BreakIndex, SrcSpan)]
filterBreaksLocations modName predicate = do
    modBreaks <- getModBreaks modName
    let breaksLocations = assocs $ modBreaks_locs modBreaks
    return $ filter predicate breaksLocations

-- | get ModBreaks for given modulename
getModBreaks :: String -> DebuggerMonad ModBreaks
getModBreaks modName = do
    module_ <- GHC.lookupModule (mkModuleName modName) Nothing
    Just modInfo <- getModuleInfo module_
    return $ modInfoModBreaks modInfo

-- | modBreaks_flags of ModBreaks contains info about set and unset breakpoints for specified module
-- | This function just set or unset given flag
changeBreakFlagInModBreaks :: String -> Int -> Bool -> DebuggerMonad Bool
changeBreakFlagInModBreaks modName flagIndex flagValue = do
    modBreaks <- getModBreaks modName
    let breaksFlags = modBreaks_flags modBreaks
    liftIO $ setBreakFlag breaksFlags flagIndex flagValue

setBreakFlag :: BreakArray -> Int -> Bool -> IO Bool
setBreakFlag bArr ind flag | flag      = setBreakOn bArr ind
                           | otherwise = setBreakOff bArr ind

-- | ':delete <module name> <break index>' command
deleteBreakpoint ::  String -> Int -> DebuggerMonad ()
deleteBreakpoint modName breakIndex = do
    res <- changeBreakFlagInModBreaks modName breakIndex False
    if res
        then printJSON [
                ("info", ConsStr "breakpoint was removed"),
                ("index", ConsInt breakIndex)
            ]
        else printJSON [
                ("info", ConsStr "breakpoint was not removed"),
                ("add_info", ConsStr "incorrect index")
            ]

-- | ':trace' command
doTrace :: String -> DebuggerMonad ()
doTrace []   = doContinue (const True) GHC.RunAndLogSteps
doTrace expr = do
    runResult <- GHC.runStmt expr GHC.RunAndLogSteps
    afterRunStmt (const True) runResult
    return ()

doContinue :: (SrcSpan -> Bool) -> SingleStep -> DebuggerMonad ()
doContinue canLogSpan step = do
    runResult <- GHC.resume canLogSpan step
    afterRunStmt canLogSpan runResult
    return ()

afterRunStmt :: (SrcSpan -> Bool) -> GHC.RunResult -> DebuggerMonad Bool
afterRunStmt _ (GHC.RunException e) = liftIO $ throwIO e
afterRunStmt canLogSpan runResult = do
    resumes <- GHC.getResumeContext
    case runResult of
        GHC.RunOk names -> do
            names_str <- mapM outToStr names
            printJSON [
                    ("info", ConsStr "finished"),
                    ("names", ConsArr $ map ConsStr names_str)
                ]
            return True
        GHC.RunBreak _ names mbBreakInfo
            | isNothing  mbBreakInfo || canLogSpan (GHC.resumeSpan $ head resumes) -> do
                names_str <- mapM outToStr names
                let srcSpan = GHC.resumeSpan $ head resumes
                functionName <- getFunctionName mbBreakInfo
                printJSON [
                        ("info", ConsStr "paused"),
                        ("src_span", srcSpanAsJSON srcSpan),
                        ("function", ConsStr functionName),
                        ("names", ConsArr $ map ConsStr names_str)
                    ]
                return False
            | otherwise -> do
                nextRunResult <- GHC.resume canLogSpan GHC.SingleStep
                afterRunStmt canLogSpan nextRunResult
        _ -> return False
        where
            getFunctionName :: Maybe BreakInfo -> DebuggerMonad String
            getFunctionName Nothing = return ""
            getFunctionName (Just breakInfo) = do
                Just modInfo <- getModuleInfo $ breakInfo_module breakInfo
                let modBreaks = modInfoModBreaks modInfo
                let allDecls = modBreaks_decls $ modBreaks
                return $ last (allDecls ! (breakInfo_number breakInfo))

-- | ':continue' command
doResume :: DebuggerMonad ()
doResume = doContinue (const True) GHC.RunToCompletion

-- |':step [<expr>]' command - general step command
doStepGeneral :: String -> DebuggerMonad ()
doStepGeneral []   = doContinue (const True) GHC.SingleStep
doStepGeneral _ = printJSON [
         ("info", ConsStr "exception"),
         ("message", ConsStr "not implemented yet")
     ]

-- |':step' command
doStepInto :: DebuggerMonad ()
doStepInto = doStepGeneral []

-- | ':steplocal' command
doStepLocal :: DebuggerMonad ()
doStepLocal = do
    mbSrcSpan <- getCurrentBreakSpan
    case mbSrcSpan of
        Nothing  -> doStepInto
        Just srcSpan -> do
            Just module_ <- getCurrentBreakModule
            mbCurrentToplevelDecl <- enclosingSpan module_ srcSpan
            case mbCurrentToplevelDecl of
                Nothing -> printJSON [
                        ("info", ConsStr "warning"),
                        ("message", ConsStr "steplocal was not performed: enclosingSpan returned Nothing")
                    ]
                Just currentToplevelDecl -> doContinue (`isSubspanOf` currentToplevelDecl) GHC.SingleStep

-- | Returns the largest SrcSpan containing the given one or Nothing
enclosingSpan :: Module -> SrcSpan -> DebuggerMonad (Maybe SrcSpan)
enclosingSpan _ (UnhelpfulSpan _) = return Nothing
enclosingSpan module_ (RealSrcSpan rSrcSpan) = do
    let line = srcSpanStartLine rSrcSpan
    breaks_ <- findBreaksContainingLine (moduleNameString $ moduleName module_) line
    case breaks_ of
        [] -> return Nothing
        bs -> return . Just . head . sortBy leftmost_largest $ enclosingSpans bs
        where
            enclosingSpans bs = [ s | (_, s) <- bs, case s of UnhelpfulSpan _ -> False
                                                              RealSrcSpan r   -> realSrcSpanEnd r >= realSrcSpanEnd rSrcSpan ]


getCurrentBreakSpan :: DebuggerMonad (Maybe SrcSpan)
getCurrentBreakSpan = do
  resumes <- GHC.getResumeContext
  case resumes of
    [] -> return Nothing
    (r:_) -> do
        let ix = GHC.resumeHistoryIx r
        if ix == 0
           then return (Just (GHC.resumeSpan r))
           else do
                let hist = GHC.resumeHistory r !! (ix-1)
                pan <- GHC.getHistorySpan hist
                return (Just pan)


getCurrentBreakModule :: DebuggerMonad (Maybe Module)
getCurrentBreakModule = do
  resumes <- GHC.getResumeContext
  case resumes of
    [] -> return Nothing
    (r:_) -> do
        let ix = GHC.resumeHistoryIx r
        if ix == 0
           then return (GHC.breakInfo_module `liftM` GHC.resumeBreakInfo r)
           else do
                let hist = GHC.resumeHistory r !! (ix-1)
                return $ Just $ GHC.getHistoryModule  hist

-- | ':history' command
showHistory :: Int -> DebuggerMonad ()
showHistory num = do
    resumes <- GHC.getResumeContext
    case resumes of
        [] -> printJSON [("info", ConsStr "not stopped at breakpoint")]
        (r:_) -> do
            let hist = resumeHistory r
                (took, rest) = splitAt num hist
            spans' <- mapM (GHC.getHistorySpan) took
            let idx = map (0-) [(1::Int) ..]
                -- todo: get full path of the file
                lines' = map (\(i, s, h) -> ConsObj [
                        ("index", ConsInt i),
                        ("function", (ConsStr . head . GHC.historyEnclosingDecls) h),
                        ("position", srcSpanAsJSON s)
                    ]) (zip3 idx spans' hist)
            printJSON [
                    ("info", ConsStr "got history"),
                    ("history", ConsArr lines'),
                    ("end_reached", ConsBool (null rest))
                ]

-- | ':breaklist' command
showBreaks :: String -> DebuggerMonad ()
showBreaks modName = do
    modBreaks <- getModBreaks modName
    let locs = assocs $ modBreaks_locs $ modBreaks
    printJSON [
            ("info", ConsStr "break list"),
            ("breaks", ConsArr $ map (\(i, e) -> ConsObj [
                    ("index", ConsInt i),
                    ("src_span", srcSpanAsJSON e)
                ]) locs)
        ]
    return ()

-- | ':print', ':sprint' and ':force' commands
doPrint, doSPrint, doForce :: String -> DebuggerMonad ()
doPrint  = pprintClosureCommand True False
doSPrint = pprintClosureCommand False False
doForce  = pprintClosureCommand False True

fullHelpText :: String
fullHelpText =
    " Commands available from the prompt:\n" ++
    " -- Commands for debugging:\n" ++
    "\n" ++
    "   :break <mod> <l>            set a breakpoint for module <mod> at the line <l>\n" ++
    "   :breaklist <mod>            show all available breakpoints (index and span) for module <mod>\n" ++
    "   :continue                   resume after a breakpoint\n" ++
    "   :delete <mod> <ind>         delete the breakpoint with index <ind> from module <mod>\n" ++
    "   :force <expr>               print <expr>, forcing unevaluated parts\n" ++
    "   :history                    after :trace, show the execution history\n" ++
    "   :print <name>               prints a value without forcing its computation\n" ++
    "   :sprint <name>              simplifed version of :print\n" ++
    "   :step                       single-step after stopping at a breakpoint\n"++
    "   :steplocal                  single-step within the current top-level binding\n"++
    "   :trace <expr>               evaluate <expr> with tracing on (see :history)\n"++
    "   :q                          exit debugger\n"

---- || Hardcoded parameters (temporary for testing) || -----------------

-- |Test module file
mainModulePath :: String
mainModulePath = "TestMainModule.hs"

-- |Test module name
mainModuleName :: String
mainModuleName = "Main"

-- |Context for test file
setupStandardContext :: DebuggerMonad ()
setupStandardContext = setupContext mainModulePath mainModuleName

-- |Default history size
defaultHistSize :: Int
defaultHistSize = 20


---- || Utils || -------------------------------------------------------

-- | Shows info from ModBreaks for given moduleName. It is useful for debuging of our debuger = )
printAllBreaksInfo :: String -> DebuggerMonad ()
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
execCommands :: [String] -> DebuggerMonad ()
execCommands []       = return ()
execCommands (c : cs) = do
    runCommand $ fst $ head $ parse debugCommand c
    execCommands cs