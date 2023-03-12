import Data.Char

-- Tip de date algebric care reprezinta un parser pt tipul A : O functie de la String la Lista de Tupluri (A, String)
newtype Parser a = Parser { apply :: String -> [(a, String)]}

-- Functie care creeaza parsere in functie de PREDICATUL (functie de la A -> Bool) pe care il dam
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred =  Parser func
        where
            func [] = []
            func (c : rest)
                | pred c = [(c, rest)]
                | otherwise = []

anychar :: Parser Char          -- parser care accepta orice caracter
anychar = satisfy $ const True

val2 = apply anychar "&ab"   -- [(&, "ab")]

char :: Char -> Parser Char     -- parser care accepta caracterul dat
char c = satisfy (== c)         

val3 = apply (char 'i') "ionel"     -- [('i',  "onel")]

digit :: Parser Char        -- parser care accepta o cifra
digit = satisfy isDigit

val4 = apply digit "1234"       --[('1', "234")]

space :: Parser Char        -- parser care accepta spatii
space = satisfy isSpace

val5 = apply space " 1234"      -- [(' ', "1234")]



--  MONADA Parser

-- instanta de FUNCTOR = defineste o functie care este aplicata pe un context inchis, ramanand in acel context

instance Functor Parser where       -- 
    fmap f parser = Parser (\input -> [(f a , rest) | (a , rest) <- apply parser input])    -- aplicam parserul pe input si pe prima valoare acceptata vom aplica f


firstToInt :: String -> Int     -- functie care primeste un string si returneaza prima cifra (Int) sau -1 daca nu este

firstToInt str = if null tuple then -1
                    else fst $ head tuple
                where tuple = apply (fmap digitToInt digit) str     


instance Applicative Parser where       -- instanta de APPLICATIVE pentru parser
    pure a = Parser (\input -> [(a, input)])        
    pf <*> pa = Parser (\input -> [(f a, resta) |
                (f, restf) <- apply pf input,
                (a ,resta) <- apply pa input ])


parseCifra  = fmap digitToInt digit             -- parser care accepta o cifra si o converteste la int
douaCifre c1 c2 = c1 * 10 + c2
parserDublu = pure douaCifre <*> parseCifra <*> parseCifra  -- parser care converteste primele 2 cifre dintr-un sir intr-un numar

val6 = apply parserDublu "1245"

endOfInput :: Parser ()
endOfInput = Parser go
            where
                go "" = [((), "")]
                go _ = []


