import Control.Monad
import Data.List
import System.Environment
import System.IO
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

data Term = Ignore | Fst | Snd | Pair [Term] [Term] | App | Curry [Term] | Var [Char]

comp :: Term -> [Term] -> [Term]
comp Ignore _ = [Ignore]
comp Fst [Pair x _] = x
comp Snd [Pair _ x] = x
comp (Pair x y) z = [Pair (foldr comp z x) $ foldr comp z y]
comp App [Pair [Curry x] y] = foldr comp [Pair [] y] x
comp (Curry x) y = [Curry $ foldr comp [Pair (foldr comp [Fst] y) [Snd]] x]
comp x y = x : y

eta :: [Term] -> [Term]
eta = opposite . (opposite >=> eta')
    where
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

parseE :: [Char] -> Either ParseError [Term]
parseE = parse (do
        whiteSpace lexer
        x <- expr []
        eof
        return x
    ) ""
    where
        lexer :: TokenParser ()
        lexer = makeTokenParser haskellStyle

        expr :: [[Char]] -> Parser [Term]
        expr ss = do
                xs <- many1 $ parens lexer (expr ss) <|> fun ss <|> var ss
                return $ foldl1 (\ x y -> comp App [Pair x y]) xs

        fun :: [[Char]] -> Parser [Term]
        fun ss = do
                symbol lexer "\\"
                ts <- many1 $ identifier lexer <|> symbol lexer "_"
                dot lexer
                x <- expr $ reverse ts ++ ss
                return $ foldr (const $ (: []) . Curry) x ts

        var :: [[Char]] -> Parser [Term]
        var ss = do
                s <- identifier lexer
                return $ maybe [Var s, Ignore] ((Snd :) . (`replicate` Fst)) $ elemIndex s ss

showE :: [Term] -> [Char]
showE x = showE' 0 x
    where
        showE' :: Int -> [Term] -> [Char]
        showE' i [App, Pair x (App : y)] = showE' i x ++ '(' : showE' i (App : y) ++ ")"
        showE' i [App, Pair x [Curry y]] = showE' i x ++ '(' : showE' i [Curry y] ++ ")"
        showE' i [App, Pair x y] = showE' i x ++ ' ' : showE' i y
        showE' i [Curry x] = '\\' : showL i x
        showE' i (Var s : _) = s
        showE' i (Snd : x) = name !! (i - 1 - length x)
        showE' i x = showE' (i - 1) $ init x

        showL :: Int -> [Term] -> [Char]
        showL i [Curry x] = name !! i ++ ' ' : showL (i + 1) x
        showL i x = name !! i ++ '.' : showE' (i + 1) x

        name :: [[Char]]
        name = "" : map show [1 ..] >>= filter (`notElem` free x) . (<$> ['a' .. 'z']) . flip (:)

        free :: [Term] -> [[Char]]
        free (App : x) = free x
        free (Pair x y : _) = free x `union` free y
        free (Curry x : _) = free x
        free (Var s : _) = [s]
        free _ = []

main :: IO ()
main = do
        args <- getArgs
        (b, args') <- return $ case args of
                "-b" : ss -> (True, ss)
                ss -> (False, ss)
        case args' of
                [] -> forever . (putStr "> " >> hFlush stdout >> getLine >>=)
                ss -> forM_ ss . (readFile >=>)
            $ either print (putStrLn . showE . if b then id else eta) . parseE
