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
expr = whiteSpace lex >> expr' >>= (eof >>) . return . bind' 0
    where
        lex :: TokenParser ()
        lex = makeTokenParser haskellStyle

        expr' :: Parser Expr
        expr' = foldl1 ((C Ev .) . P) <$> many1 (parens lex expr' <|> fun <|> var)

        fun :: Parser Expr
        fun = lexeme lex (char '\\') >> many (identifier lex) >>= (<$> (lexeme lex (char '.') >> expr')) . flip (foldr $ (L .) . bind 0)

        var :: Parser Expr
        var = V <$> identifier lex

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

showExpr :: Expr -> [Char]
showExpr x = showExpr' 0 x
    where
        showExpr' :: Int -> Expr -> [Char]
        showExpr' i (C Ev (P x (C Ev y))) = showExpr' i x ++ '(' : showExpr' i (C Ev y) ++ ")"
        showExpr' i (C Ev (P x (L y))) = showExpr' i x ++ '(' : showExpr' i (L y) ++ ")"
        showExpr' i (C Ev (P x y)) = showExpr' i x ++ ' ' : showExpr' i y
        showExpr' i (C x _) = showExpr' (i - 1) x
        showExpr' i (L x) = '\\' : name !! i ++ '.' : showExpr' (i + 1) x
        showExpr' i (V s) = s
        showExpr' i _ = name !! (i - 1)

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
    hSetBuffering stdout NoBuffering
    case args' of
        [] -> forever . (putStr "> " >> getLine >>=)
        ss -> forM_ ss . (readFile >=>)
        $ either print (putStrLn . showExpr . red) . parse expr ""
