import Control.Monad
import Data.List
import System.Environment
import System.IO
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

data Expr = P1 | P2 | P [Expr] [Expr] | Ev | L [Expr] | V [Char]

c :: Expr -> [Expr] -> [Expr]
c P1 [P x _] = x
c P2 [P _ x] = x
c (P x y) z = [P (foldr c z x) $ foldr c z y]
c Ev [P [L x] y] = foldr c [P [] y] x
c (L x) y = [L $ foldr c [P (foldr c [P1] y) [P2]] x]
c x y = x : y

eta :: [Expr] -> [Expr]
eta = reverseE . (reverseE >=> eta')
    where
        eta' :: Expr -> [Expr]
        eta' (P x y) = case (x >>= eta', y >>= eta') of
                (P1 : x, P1 : y) -> P1 : eta' (P x y)
                (x, y) -> [P x y]
        eta' (L x) = case x >>= eta' of
                [P (P1 : x) [P2], Ev] -> x
                x -> [L x]
        eta' x = [x]

        reverseE :: [Expr] -> [Expr]
        reverseE = reverse . map reverseE'
        
        reverseE' :: Expr -> Expr
        reverseE' (P x y) = P (reverseE x) $ reverseE y
        reverseE' (L x) = L $ reverseE x
        reverseE' x = x

parseE :: [Char] -> Either ParseError [Expr]
parseE = parse (do
        whiteSpace lexer
        x <- expr []
        eof
        return x
    ) ""
    where
        lexer :: TokenParser ()
        lexer = makeTokenParser haskellStyle

        expr :: [[Char]] -> Parser [Expr]
        expr ss = do
                xs <- many1 $ parens lexer (expr ss) <|> fun ss <|> var ss
                return $ foldl1 (\ x y -> c Ev [P x y]) xs

        fun :: [[Char]] -> Parser [Expr]
        fun ss = do
                symbol lexer "\\"
                ts <- many $ identifier lexer <|> symbol lexer "_"
                dot lexer
                x <- expr $ reverse ts ++ ss
                return $ foldr (const $ (: []) . L) x ts

        var :: [[Char]] -> Parser [Expr]
        var ss = do
                s <- identifier lexer
                return $ maybe (V s : map (const P1) ss) ((P2 :) . (`replicate` P1)) $ elemIndex s ss

showE :: [Expr] -> [Char]
showE x = showE' 0 x
    where
        showE' :: Int -> [Expr] -> [Char]
        showE' i [Ev, P x (Ev : y)] = showE' i x ++ '(' : showE' i (Ev : y) ++ ")"
        showE' i [Ev, P x [L y]] = showE' i x ++ '(' : showE' i [L y] ++ ")"
        showE' i [Ev, P x y] = showE' i x ++ ' ' : showE' i y
        showE' i [L x] = '\\' : showL i x
        showE' i (V s : _) = s
        showE' i (P2 : x) = name !! (i - 1 - length x)
        showE' i x = showE' (i - 1) $ init x

        showL :: Int -> [Expr] -> [Char]
        showL i [L x] = name !! i ++ ' ' : showL (i + 1) x
        showL i x = name !! i ++ '.' : showE' (i + 1) x

        name :: [[Char]]
        name = "" : map show [1 ..] >>= filter (`notElem` free x) . (<$> ['a' .. 'z']) . flip (:)

        free :: [Expr] -> [[Char]]
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
            $ either print (putStrLn . showE . if b then id else eta) . parseE
