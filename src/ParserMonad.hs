module ParserMonad where

import Data.Char

-- | Parser type
newtype Parser a = P (String -> [ (a, String) ])

-- | parse function
parse :: Parser a -> String -> [ (a, String) ]
parse (P p) s = p s

-- | conveyer of parsers
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = P $ \cs -> case parse p1 cs of
    [] -> parse p2 cs
    ps -> ps

instance Monad Parser where
    p1 >>= fp2 = P $ \cs -> do
        (a, cs') <- parse p1 cs
        parse (fp2 a) cs'
    return x   = P $ \cs -> [ (x, cs) ]
    fail _     = P $ \_ ->  []

instance Functor Parser where
    fmap f p = p >>= \x -> return (f x)

-- | Parses one char
anyChar :: Parser Char
anyChar = P $ \cs -> case cs of
    (x:xs) -> [ (x, xs) ]
    []     -> []

-- | Parses char by predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    c <- anyChar
    if p c then return c else fail "Did not satisfy boolean predicate"

-- | Parses exact Char
char :: Char -> Parser Char
char = satisfy . (==)

-- | Parses exact String
string :: String -> Parser String
string = mapM char

-- | Parses zero or more instances of p.
many :: Parser a -> Parser [a]
many p = rest p <|> (return [])

-- | Parses one or more instances of p.
rest :: Parser a -> Parser [a]
rest p = do
    x  <- p
    xs <- many p
    return (x:xs)

-- | Parses one special character
alpha, digit, upper, lower, space :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpace

-- | Parses integer
int :: Parser Int
int = do
    s <- string "-" <|> return []
    d <- rest digit
    return (read (s ++ d) :: Int)

-- | Parses sequence of 'ok' chars
manySatisfiedChars :: (Char -> Bool) -> Parser String
manySatisfiedChars = many . satisfy
restSatisfiedChars :: (Char -> Bool) -> Parser String
restSatisfiedChars = rest . satisfy

-- | Parses word of non-space characters

-- | Skips spacing characters
skipSpaces :: Parser ()
skipSpaces = do
    manySatisfiedChars isSpace
    return ()

-- | Skips spacing characters (more than zero)
waitAndSkipSpaces :: Parser ()
waitAndSkipSpaces = do
    restSatisfiedChars isSpace
    return ()

-- | Returns remaining input
restOfInput :: Parser String
restOfInput = manySatisfiedChars $ const True


-- | Fails if end was not reached
end :: Parser ()
end = do
    b <- do {
             c <- anyChar ;
             return False ;
         } <|> return True
    if b then return () else fail "End wasn't reached"

-- | Connects all parsers
unlist :: [Parser a] -> Parser a
unlist = foldr (<|>) (fail "Could not parse")