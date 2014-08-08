module DebuggerUtils where

import Outputable (Outputable)
import qualified Outputable
import DebuggerMonad
import GHC hiding (resume)
import DynFlags
import GhcMonad (liftIO)
import qualified FastString
import Text.JSON
import qualified Data.Ratio
import Name

compareNames :: Name -> Name -> Ordering
n1 `compareNames` n2 = compareWith n1 `compare` compareWith n2
    where compareWith n = (getOccString n, getSrcSpan n)

-- | Prints SDoc to the debug stream
printSDoc :: Outputable.SDoc -> DebuggerMonad ()
printSDoc message = do
    st <- getDebugState
    dflags <- getDynFlags
    unqual <- getPrintUnqual
    liftIO $ Outputable.printForUser dflags (debugOutput st) unqual message
    return ()

showSDoc :: Outputable.SDoc -> DebuggerMonad String
showSDoc message = do
    dflags <- getDynFlags
    unqual <- getPrintUnqual
    return $ Outputable.showSDocForUser dflags unqual message

showOutputable :: (Outputable d) => d -> DebuggerMonad String
showOutputable str = do
    showSDoc $ Outputable.ppr str

-- | Prints Outputable to the debug stream
printOutputable :: (Outputable d) => d -> DebuggerMonad ()
printOutputable = printSDoc . Outputable.ppr

printListOfOutputable :: (Outputable d) => [d] -> DebuggerMonad ()
printListOfOutputable = mapM_ printOutputable

-- | Prints String to the debug stream
printString :: String -> DebuggerMonad ()
printString = printSDoc . Outputable.text

printJSON :: [(String, T)] -> DebuggerMonad ()
printJSON = printString . getJSONLine


data T
    = ConsNull
    | ConsBool Bool
    | ConsInt Int
    | ConsStr String
    | ConsArr [T]
    | ConsObj [(String, T)]

instance JSON T where
    readJSON _ = Error "Not implemented"    -- function is not needed

    showJSON (ConsNull) = JSNull
    showJSON (ConsBool x) = JSBool x
    showJSON (ConsInt x) = JSRational False ((toInteger x) Data.Ratio.% 1)
    showJSON (ConsStr x) = JSString $ toJSString x
    showJSON (ConsArr x) = JSArray $ map showJSON x
    showJSON (ConsObj x) = JSObject $ toJSObject $ map (\(key, value) -> (key, showJSON value)) x


getJSONLine :: [(String, T)] -> String
getJSONLine = encode . toJSObject

-- | Used methods are marked as 'unsafe'
srcSpanAsJSON :: SrcSpan -> T
srcSpanAsJSON (UnhelpfulSpan str) = ConsObj [("span", ConsStr $ FastString.unpackFS str)]
srcSpanAsJSON (RealSrcSpan span') = ConsObj [
        ("file", ConsStr $ FastString.unpackFS $ srcSpanFile span'),
        ("startline", ConsInt $ srcSpanStartLine span'),
        ("endline", ConsInt $ srcSpanEndLine span'),
        ("startcol", ConsInt $ srcSpanStartCol span'),
        ("endcol", ConsInt $ srcSpanEndCol span' - 1)
    ]
