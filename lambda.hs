import Control.Monad
import Data.List
import System.Environment
import System.IO
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

data Term = P1 | P2 | P [Term] [Term] | Ev | L [Term] | V [Char]

c :: Term -> [Term] -> [Term]
c P1 [P x _] = x
c P2 [P _ x] = x
c (P x y) z = [P (foldr c z x) $ foldr c z y]
c Ev [P [L x] y] = foldr c [P [] y] x
c (L x) y = [L $ foldr c [P (foldr c [P1] y) [P2]] x]
c x y = x : y

eta :: [Term] -> [Term]
eta = reverseT . (reverseT >=> eta')
    where
        eta' :: Term -> [Term]
        eta' (P x y) = case (x >>= eta', y >>= eta') of
                (P1 : x, P1 : y) -> P1 : eta' (P x y)
                (x, y) -> [P x y]
        eta' (L x) = case x >>= eta' of
                [P (P1 : x) [P2], Ev] -> x
                x -> [L x]
        eta' x = [x]

        reverseT :: [Term] -> [Term]
        reverseT = reverse . map reverseT'
        
        reverseT' :: Term -> Term
        reverseT' (P x y) = P (reverseT x) $ reverseT y
        reverseT' (L x) = L $ reverseT x
        reverseT' x = x

parseT :: [Char] -> Either ParseError [Term]
parseT = parse (do
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
                return $ foldl1 (\ x y -> c Ev [P x y]) xs

        fun :: [[Char]] -> Parser [Term]
        fun ss = do
                symbol lexer "\\"
                ts <- many1 $ identifier lexer <|> symbol lexer "_"
                dot lexer
                x <- expr $ reverse ts ++ ss
                return $ foldr (const $ (: []) . L) x ts

        var :: [[Char]] -> Parser [Term]
        var ss = do
                s <- identifier lexer
                return $ maybe (V s : map (const P1) ss) ((P2 :) . (`replicate` P1)) $ elemIndex s ss

showT :: [Term] -> [Char]
showT x = showT' 0 x
    where
        showT' :: Int -> [Term] -> [Char]
        showT' i [Ev, P x (Ev : y)] = showT' i x ++ '(' : showT' i (Ev : y) ++ ")"
        showT' i [Ev, P x [L y]] = showT' i x ++ '(' : showT' i [L y] ++ ")"
        showT' i [Ev, P x y] = showT' i x ++ ' ' : showT' i y
        showT' i [L x] = '\\' : showL i x
        showT' i (V s : _) = s
        showT' i (P2 : x) = name !! (i - 1 - length x)
        showT' i x = showT' (i - 1) $ init x

        showL :: Int -> [Term] -> [Char]
        showL i [L x] = name !! i ++ ' ' : showL (i + 1) x
        showL i x = name !! i ++ '.' : showT' (i + 1) x

        name :: [[Char]]
        name = "" : map show [1 ..] >>= filter (`notElem` free x) . (<$> ['a' .. 'z']) . flip (:)

        free :: [Term] -> [[Char]]
        free (Ev : x) = free x
        free (P x y : _) = free x `union` free y
        free (L x : _) = free x
        free (V s : _) = [s]
        free _ = []

main :: IO ()
main = do
        args <- getArgs
        (b, args') <- return $ case args of
                s : ss | s == "-b" -> (True, ss)
                ss -> (False, ss)
        case args' of
                [] -> forever . (putStr "> " >> hFlush stdout >> getLine >>=)
                ss -> forM_ ss . (readFile >=>)
            $ either print (putStrLn . showT . if b then id else eta) . parseT
