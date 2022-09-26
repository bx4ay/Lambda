import Data.List
import System.Environment
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

data Expr = C Expr Expr | Id | P Expr Expr | P1 | P2 | L Expr | Ev | V [Char]

eval :: Expr -> Expr
eval (L x) = L $ eval x
eval (C x y) = c (eval x) $ eval y
    where
        c :: Expr -> Expr -> Expr
        c (C x y) z = c x $ c y z
        c Id x = x
        c x Id = x
        c P1 (P x _) = x
        c P2 (P _ x) = x
        c (P x y) z = P (c x z) $ c y z
        c Ev (P (L x) y) = c x $ P Id y
        c (L x) y = L . c x $ P (c y P1) P2
        c (V s) _ = V s
        c x y = C x y
eval (P x y) = P (eval x) $ eval y
eval x = x

parser :: Parser Expr
parser = do
    whiteSpace lexer
    x <- expr
    eof
    return x
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

show' :: Expr -> [Char]
show' x = show'' (filter (`notElem` free x) . concatMap ((<$> ['a' .. 'z']) . flip (:)) $ "" : map show [1 ..]) 0 x
    where
        show'' :: [[Char]] -> Int -> Expr -> [Char]
        show'' l i (C Ev (P x (C Ev y))) = show'' l i x ++ '(' : show'' l i (C Ev y) ++ ")"
        show'' l i (C Ev (P x (L y))) = show'' l i x ++ '(' : show'' l i (L y) ++ ")"
        show'' l i (C Ev (P x y)) = show'' l i x ++ ' ' : show'' l i y
        show'' l i (L x) = '\\' : l !! i ++ '.' : show'' l (i + 1) x
        show'' l i (V s) = s
        show'' l i (C _ x) = show'' l (i - 1) x
        show'' l i x = l !! (i - 1)

        free :: Expr -> [[Char]]
        free (C Ev (P x y)) = free x `union` free y
        free (L x) = free x
        free (V s) = [s]
        free _ = []

main :: IO ()
main = do
    args <- getArgs
    if null args then i else either print (putStrLn . show' . eval) . parse parser "" =<< readFile (head args)
    where
        i :: IO ()
        i = do
            putStr "> "
            either print (putStrLn . show' . eval) . parse parser "" =<< getLine
            i
