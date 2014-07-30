module DebuggerUtils where

import Outputable (Outputable)
import qualified Outputable
import DebuggerMonad
import GHC hiding (resume)
import DynFlags
import GhcMonad (liftIO)
import ParserMonad (parse)
import BreakArray
import Data.Array
import qualified FastString
import Text.JSON
import qualified Data.Ratio

-- | Prints SDoc to the debug stream
printSDoc :: Outputable.SDoc -> DebuggerMonad ()
printSDoc message = do
    st <- getDebugState
    dflags <- getDynFlags
    unqual <- getPrintUnqual
    liftIO $ Outputable.printForUser dflags (debugOutput st) unqual message
    return ()

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

outToStr :: (Outputable d) => d -> DebuggerMonad String
outToStr str = do
    dflags <- getDynFlags
    return $ Outputable.showSDoc dflags $ Outputable.ppr str

data T
    = ConsBool Bool
    | ConsInt Int
    | ConsStr String
    | ConsArr [T]
    | ConsObj [(String, T)]

instance JSON T where
    readJSON _ = Error "Not implemented"    -- function is not needed
--    readJSON (JSRational b x) = if Data.Ratio.denominator x == 1 then Ok (ConsInt $ Data.Ratio.numerator x) else Error "Non-integer number"
--    readJSON (JSString x) = Ok (ConsStr $ fromJSString x)
--    readJSON (JSArray []) = Ok (ConsArr [])
--    readJSON (JSArray (x:xs)) = conc (readJSON x) (readJSON (JSArray xs)) where
--        conc a b = case a of
--            Error er -> Error er
--            Ok ok -> case b of
--                Error er -> Error er
--                Ok (ConsArr oks) -> Ok $ ConsArr (ok:oks)
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
        ("startlcol", ConsInt $ srcSpanStartCol span'),
        ("endcol", ConsInt $ srcSpanEndCol span')
    ]
