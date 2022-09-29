import Control.Monad
import Data.List
import System.Environment
import System.IO
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

data Expr = C Expr Expr | Id | P Expr Expr | P1 | P2 | L Expr | Ev | V [Char]

beta :: Expr -> Expr
beta (C x y) = case (beta x, beta y) of
        (C x y, z) -> beta . C x . beta $ C y z
        (Id, x) -> x
        (x, Id) -> x
        (P1, P x _) -> x
        (P2, P _ x) -> x
        (P x y, z) -> P (beta $ C x z) . beta $ C y z
        (Ev, P (L x) y) -> beta . C x $ P Id y
        (L x, y) -> L . beta . C x $ P (beta $ C y P1) P2
        (x, y) -> C x y
beta (P x y) = P (beta x) $ beta y
beta (L x) = L $ beta x
beta x = x

eta :: Expr -> Expr
eta (C x y) = case eta y of
        C y z -> C (eta $ C x y) z
        y -> C (eta x) y
eta (P x y) = case (eta x, eta y) of
        (C x P1, C y P1) -> C (eta $ P x y) P1
        (x, y) -> P x y
eta (L x) = case eta x of
        C Ev (P (C x P1) P2) -> x
        x -> L x
eta x = x

expr :: Parser Expr
expr = do
        whiteSpace lex
        x <- expr'
        eof
        return $ bind' 0 x
    where
        lex :: TokenParser ()
        lex = makeTokenParser haskellStyle

        expr' :: Parser Expr
        expr' = do
                ss <- many1 (parens lex expr' <|> fun <|> var)
                return $ foldl1 ((C Ev .) . P) ss

        fun :: Parser Expr
        fun = do
                symbol lex "\\"
                ss <- many (identifier lex <|> symbol lex "_")
                dot lex
                x <- expr'
                return $ foldr ((L .) . bind 0) x ss

        var :: Parser Expr
        var = do
                s <- identifier lex
                return $ V s

        bind :: Int -> [Char] -> Expr -> Expr
        bind i s (C Ev (P x y)) = C Ev . P (bind i s x) $ bind i s y
        bind i s (L x) = L $ bind (i + 1) s x
        bind i s (V t) | s == t = C P2 $ iterate (C P1) Id !! i
        bind _ _ x = x

        bind' :: Int -> Expr -> Expr
        bind' i (C Ev (P x y)) = C Ev . P (bind' i x) $ bind' i y
        bind' i (L x) = L $ bind' (i + 1) x
        bind' i (V t) = C (V t) $ iterate (C P1) Id !! i
        bind' _ x = x

showE :: Expr -> [Char]
showE x = showE' 0 x
    where
        showE' :: Int -> Expr -> [Char]
        showE' i (C Ev (P x (C Ev y))) = showE' i x ++ '(' : showE' i (C Ev y) ++ ")"
        showE' i (C Ev (P x (L y))) = showE' i x ++ '(' : showE' i (L y) ++ ")"
        showE' i (C Ev (P x y)) = showE' i x ++ ' ' : showE' i y
        showE' i (C (V s) _) = s
        showE' i (C x P1) = showE' (i - 1) x
        showE' i (C _ x) = showE' (i - 1) x
        showE' i (L x) = '\\' : showL i x
        showE' i (V s) = s
        showE' i _ = name !! (i - 1)

        showL :: Int -> Expr -> [Char]
        showL i (L x) = name !! i ++ ' ' : showL (i + 1) x
        showL i x = name !! i ++ '.' : showE' (i + 1) x

        name :: [[Char]]
        name = "" : map show [1 ..] >>= filter (`notElem` free x) . (<$> ['a' .. 'z']) . flip (:)

        free :: Expr -> [[Char]]
        free (C Ev x) = free x
        free (C x _) = free x
        free (P x y) = free x `union` free y
        free (L x) = free x
        free (V s) = [s]
        free _ = []

main :: IO ()
main = do
        args <- getArgs
        (red, args') <- return $ case args of
                s : ss | s == "-b" -> (beta, ss)
                ss -> (eta . beta, ss)
        case args' of
                [] -> forever . (putStr "> " >> hFlush stdout >> getLine >>=)
                ss -> forM_ ss . (readFile >=>)
            $ either print (putStrLn . showE . red) . parse expr ""
