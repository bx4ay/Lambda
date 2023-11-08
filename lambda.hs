import Control.Monad
import Data.List (elemIndex, union)
import Data.Map (Map, (!?), empty, insert)
import System.Environment
import System.IO
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

data Term = Ignore | Fst | Snd | Pair [Term] [Term] | App | Curry [Term] | Free [Char]

comp :: Term -> [Term] -> [Term]
comp Ignore _ = [Ignore]
comp Fst [Pair x _] = x
comp Snd [Pair _ x] = x
comp (Pair x y) z = [Pair (foldr comp z x) $ foldr comp z y]
comp App [Pair [Curry x] y] = foldr comp [Pair [] y] x
comp (Curry x) y = [Curry $ foldr comp [Pair (foldr comp [Fst] y) [Snd]] x]
comp x y = x : y

eta :: [Term] -> [Term]
eta = opposite . (opposite >=> eta') where

    eta' :: Term -> [Term]
    eta' (Pair x y) = case (x >>= eta', y >>= eta') of
        (Ignore : x, Ignore : y) -> [Ignore, Pair x y]
        (Ignore : x, Fst : y) -> Fst : eta' (Pair (Ignore : x) y)
        (Fst : x, Ignore : y) -> Fst : eta' (Pair x $ Ignore : y)
        (Fst : x, Fst : y) -> Fst : eta' (Pair x y)
        (x, y) -> [Pair x y]
    eta' (Curry x) = case x >>= eta' of
        [Pair (Ignore : x) [Snd], App] -> Ignore : x
        [Pair (Fst : x) [Snd], App] -> x
        x -> [Curry x]
    eta' x = [x]

    opposite :: [Term] -> [Term]
    opposite = reverse . map inverse

    inverse :: Term -> Term
    inverse (Pair x y) = Pair (opposite x) $ opposite y
    inverse (Curry x) = Curry $ opposite x
    inverse x = x

parseE :: Map [Char] [Term] -> [Char] -> Either ParseError (Map [Char] [Term], Maybe [Term])
parseE defs = parse (try (def defs) <|> nodef defs) "" where

    lexer :: TokenParser ()
    lexer = makeTokenParser haskellStyle

    ident :: Parsec [Char] () Char -> Parsec [Char] () [Char]
    ident lu = do
        c <- lu
        cs <- many $ alphaNum <|> oneOf "_'"
        spaces
        return $ c : cs

    def :: Map [Char] [Term] -> Parser (Map [Char] [Term], Maybe [Term])
    def defs = do
        whiteSpace lexer
        id <- ident upper
        symbol lexer "="
        x <- expr defs []
        eof
        return (insert id x defs, Nothing)

    nodef :: Map [Char] [Term] -> Parser (Map [Char] [Term], Maybe [Term])
    nodef defs = do
        whiteSpace lexer
        x <- expr defs []
        eof
        return (defs, Just x)

    expr :: Map [Char] [Term] -> [[Char]] -> Parser [Term]
    expr defs ids = do
        xs <- many1 $ parens lexer (expr defs ids) <|> fun defs ids <|> var ids <|> alias defs
        return $ foldl1 (\ x y -> comp App [Pair x y]) xs

    fun :: Map [Char] [Term] -> [[Char]] -> Parser [Term]
    fun defs ids = do
        symbol lexer "\\"
        ids' <- many1 $ ident lower <|> symbol lexer "_"
        dot lexer
        x <- expr defs $ reverse ids' ++ ids
        return $ foldr (const $ (: []) . Curry) x ids'

    var :: [[Char]] -> Parser [Term]
    var ids = do
        id <- ident lower
        return $ maybe [Free id, Ignore] ((Snd :) . (`replicate` Fst)) $ elemIndex id ids

    alias :: Map [Char] [Term] -> Parser [Term]
    alias defs = do
        id' <- ident upper
        maybe (errorWithoutStackTrace $ id' ++ "is undefined") return $ defs !? id'

showE :: [Term] -> [Char]
showE x = showE' 0 x where

    showE' :: Int -> [Term] -> [Char]
    showE' i [App, Pair x (App : y)] = showE' i x ++ '(' : showE' i (App : y) ++ ")"
    showE' i [App, Pair x [Curry y]] = showE' i x ++ '(' : showE' i [Curry y] ++ ")"
    showE' i [App, Pair x y] = showE' i x ++ ' ' : showE' i y
    showE' i [Curry x] = '\\' : showL i x
    showE' i (Free s : _) = s
    showE' i (Snd : x) = names !! (i - 1 - length x)
    showE' i x = showE' (i - 1) $ init x

    showL :: Int -> [Term] -> [Char]
    showL i [Curry x] = names !! i ++ ' ' : showL (i + 1) x
    showL i x = names !! i ++ '.' : showE' (i + 1) x

    names :: [[Char]]
    names = "" : map show [1 ..] >>= filter (`notElem` free x) . (<$> ['a' .. 'z']) . flip (:)

    free :: [Term] -> [[Char]]
    free (App : x) = free x
    free (Pair x y : _) = free x `union` free y
    free (Curry x : _) = free x
    free (Free s : _) = [s]
    free _ = []

main :: IO ()
main = do
    args <- getArgs
    f (args == ["-b"]) empty where

    f :: Bool -> Map [Char] [Term] -> IO ()
    f b defs = do
        putStr "> "
        hFlush stdout
        s <- getLine
        either print (uncurry $ g b) $ parseE defs s

    g :: Bool -> Map [Char] [Term] -> Maybe [Term] -> IO ()
    g b defs Nothing = f b defs
    g b defs (Just x) = do
        putStrLn $ showE $ if b then x else eta x
        f b defs
