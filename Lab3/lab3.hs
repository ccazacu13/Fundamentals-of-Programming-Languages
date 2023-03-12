import Data.Char

newtype Parser a = Parser {apply :: String -> [(a, String)]}

satisfy :: (Char -> Bool) -> Parser Char

satisfy pred = Parser go
        where 
            go [] = []
            go (c : str)
                | pred c = [(c, str)]
                | otherwise = []

instance Functor Parser where
    fmap f pa = Parser ( \input -> [(f a, rest) | (a, rest) <- apply pa input])

instance Applicative Parser where
    pure a = Parser (\input -> [(a, input)])
    pf <*> pa = Parser (\input -> [(f a, resta) |
                    (f, restf) <- apply pf input,
                    (a, resta) <- apply pa input])

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= k = Parser (\input -> [(b, restb) |
                (a, resta) <- apply pa input,
                (b, restb) <- apply (k a) resta])

-- parser care sa gaseasca cifrele din paranteze

cifra :: Parser Char
cifra = satisfy isDigit

char :: Char -> Parser Char
char c = satisfy (==c)

parsezParanteze :: Parser Char
parsezParanteze = do
                par <- char '('
                return par
                -- cif <- cifra
                -- char ')'
                -- return $ digitToInt cif


-- parser care identifica cifra precedata de "+-"

recunosc :: Parser Char
recunosc = satisfy $ \c -> c `elem` "+-"

returnezPlusMinus :: Parser Int
returnezPlusMinus = do
                semn <- recunosc
                cif <- cifra
                if semn == '+' then
                    return $ digitToInt cif
                else
                    return $ (-1) * (digitToInt cif)

-- convert '+' d = d
-- convert '-' d = -d

-- returnApp :: Parser Int
-- returnApp =  pure convert <*> satisfy (\x -> elem x "+-") <*> (fmap digitToInt cifra)

convert '+' d = d
convert '-' d = -d

cifSemn' :: Parser Int
cifSemn' =
    pure convert <*> satisfy (\x -> elem x "+-") <*> (digitToInt <$> cifra)



-- parser care identifica un keyword

keyword :: String -> Parser String
keyword [] = return []
keyword (x : xs) = do
            char x
            keyword xs
            return (x : xs)