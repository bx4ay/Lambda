import Data.List
import System.Environment
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

parser :: Parser Expr
parser = do
    whiteSpace lexer
    x <- expr
    eof
    return $ lambda' 0 x
    where
        lexer :: TokenParser ()
        lexer = makeTokenParser emptyDef {identStart = letter <|> char '_', identLetter = alphaNum <|> char '_'}

        expr :: Parser Expr
        expr = foldl1 ((C Ev .) . P) <$> many1 (parens lexer expr <|> V <$> identifier lexer <|> do
            lexeme lexer $ char '\\'
            s <- identifier lexer
            lexeme lexer $ char '.'
            L . lambda 0 s <$> expr)

        lambda :: Int -> [Char] -> Expr -> Expr
        lambda i s (C Ev (P x y)) = C Ev . P (lambda i s x) $ lambda i s y
        lambda i s (L x) = L $ lambda (i + 1) s x
        lambda i s (V t) | s == t = C P2 $ iterate (C P1) Id !! i
        lambda _ _ x = x

        lambda' :: Int -> Expr -> Expr
        lambda' i (C Ev (P x y)) = C Ev . P (lambda' i x) $ lambda' i y
        lambda' i (L x) = L $ lambda' (i + 1) x
        lambda' i (V t) = C (V t) $ iterate (C P1) Id !! i
        lambda' _ x = x

show' :: Expr -> [Char]
show' x = show'' (filter (`notElem` free x) . concatMap ((<$> ['a' .. 'z']) . flip (:)) $ "" : map show [1 ..]) 0 x
    where
        show'' :: [[Char]] -> Int -> Expr -> [Char]
        show'' l i (C Ev (P x (C Ev y))) = show'' l i x ++ '(' : show'' l i (C Ev y) ++ ")"
        show'' l i (C Ev (P x (L y))) = show'' l i x ++ '(' : show'' l i (L y) ++ ")"
        show'' l i (C Ev (P x y)) = show'' l i x ++ ' ' : show'' l i y
        show'' l i (C x _) = show'' l (i - 1) x
        show'' l i (L x) = '\\' : l !! i ++ '.' : show'' l (i + 1) x
        show'' l i (V s) = s
        show'' l i x = l !! (i - 1)

        free :: Expr -> [[Char]]
        free (C x y) = free x `union` free y
        free (P x y) = free x `union` free y
        free (L x) = free x
        free (V s) = [s]
        free _ = []

main :: IO ()
main = do
    args <- getArgs
    if null args then i else either print (putStrLn . show' . eta . beta) . parse parser "" =<< readFile (head args)
    where
        i :: IO ()
        i = do
            putStr "> "
            either print (putStrLn . show' . eta . beta) . parse parser "" =<< getLine
            i
