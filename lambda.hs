import Control.Monad
import Data.List
import System.Environment
import System.IO
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

data Term = U | F | S | P [Term] [Term] | A | L [Term] | V [Char]

c :: Term -> [Term] -> [Term]
c U _ = [U]
c F [P x _] = x
c S [P _ x] = x
c (P x y) z = [P (foldr c z x) $ foldr c z y]
c A [P [L x] y] = foldr c [P [] y] x
c (L x) y = [L $ foldr c [P (foldr c [F] y) [S]] x]
c x y = x : y

eta :: [Term] -> [Term]
eta = reverseE . (reverseE >=> eta')
    where
        eta' :: Term -> [Term]
        eta' (P x y) = case (x >>= eta', y >>= eta') of
                (U : x, U : y) -> [U, P x y]
                (U : x, F : y) -> F : eta' (P (U : x) y)
                (F : x, U : y) -> F : eta' (P x $ U : y)
                (F : x, F : y) -> F : eta' (P x y)
                (x, y) -> [P x y]
        eta' (L x) = case x >>= eta' of
                [P (U : x) [S], A] -> U : x
                [P (F : x) [S], A] -> x
                x -> [L x]
        eta' x = [x]

        reverseE :: [Term] -> [Term]
        reverseE = reverse . map reverseT
        
        reverseT :: Term -> Term
        reverseT (P x y) = P (reverseE x) $ reverseE y
        reverseT (L x) = L $ reverseE x
        reverseT x = x

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
                return $ foldl1 (\ x y -> c A [P x y]) xs

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
                return $ maybe [V s, U] ((S :) . (`replicate` F)) $ elemIndex s ss

showE :: [Term] -> [Char]
showE x = showE' 0 x
    where
        showE' :: Int -> [Term] -> [Char]
        showE' i [A, P x (A : y)] = showE' i x ++ '(' : showE' i (A : y) ++ ")"
        showE' i [A, P x [L y]] = showE' i x ++ '(' : showE' i [L y] ++ ")"
        showE' i [A, P x y] = showE' i x ++ ' ' : showE' i y
        showE' i [L x] = '\\' : showL i x
        showE' i (V s : _) = s
        showE' i (S : x) = name !! (i - 1 - length x)
        showE' i x = showE' (i - 1) $ init x

        showL :: Int -> [Term] -> [Char]
        showL i [L x] = name !! i ++ ' ' : showL (i + 1) x
        showL i x = name !! i ++ '.' : showE' (i + 1) x

        name :: [[Char]]
        name = "" : map show [1 ..] >>= filter (`notElem` free x) . (<$> ['a' .. 'z']) . flip (:)

        free :: [Term] -> [[Char]]
        free (A : x) = free x
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
            $ either print (putStrLn . showE . if b then id else eta) . parseE
