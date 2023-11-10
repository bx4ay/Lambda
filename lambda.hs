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
    opposite = reverse . map opposite'

    opposite' :: Term -> Term
    opposite' (Pair x y) = Pair (opposite x) $ opposite y
    opposite' (Curry x) = Curry $ opposite x
    opposite' x = x

parseExpr :: Map [Char] [Term] -> [Char] -> Either ParseError (Map [Char] [Term], Maybe [Term])
parseExpr defs = parse (try (def defs) <|> nodef defs) "" where

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
        s <- ident upper
        symbol lexer "="
        x <- expr defs []
        eof
        return (insert s x defs, Nothing)

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
        ss <- many1 $ ident lower <|> symbol lexer "_"
        dot lexer
        x <- expr defs $ reverse ss ++ ids
        return $ foldr (const $ (: []) . Curry) x ss

    var :: [[Char]] -> Parser [Term]
    var ids = do
        s <- ident lower
        return $ maybe [Free s, Ignore] ((Snd :) . (`replicate` Fst)) $ elemIndex s ids

    alias :: Map [Char] [Term] -> Parser [Term]
    alias defs = do
        s <- ident upper
        maybe (errorWithoutStackTrace $ s ++ "is undefined") return $ defs !? s

showExpr :: [Term] -> [Char]
showExpr x = showExpr' 0 x where

    showExpr' :: Int -> [Term] -> [Char]
    showExpr' i [App, Pair x (App : y)] = showExpr' i x ++ "(" ++ showExpr' i (App : y) ++ ")"
    showExpr' i [App, Pair x [Curry y]] = showExpr' i x ++ "(" ++ showExpr' i [Curry y] ++ ")"
    showExpr' i [App, Pair x y] = showExpr' i x ++ " " ++ showExpr' i y
    showExpr' i [Curry x] = "\\" ++ ids !! i ++ "." ++ showExpr' (i + 1) x
    showExpr' i (Free s : _) = s
    showExpr' i (Snd : x) = ids !! (i - 1 - length x)
    showExpr' i x = showExpr' (i - 1) $ init x

    ids :: [[Char]]
    ids = "" : map show [1 ..] >>= filter (`notElem` free x) . (<$> ['a' .. 'z']) . flip (:)

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
        either (errorWithoutStackTrace . show) (uncurry $ g b) $ parseExpr defs s

    g :: Bool -> Map [Char] [Term] -> Maybe [Term] -> IO ()
    g b defs Nothing = f b defs
    g b defs (Just x) = do
        putStrLn $ showExpr $ if b then x else eta x
        f b defs
