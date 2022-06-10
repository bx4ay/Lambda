import Data.Char (isAlpha, isAlphaNum)
import System.Environment (getArgs)

data Expr = V [Char] | L [Char] Expr | A Expr Expr

app :: Expr -> Expr -> Expr
app (L x m) n = subst n x m
    where
        subst :: Expr -> [Char] -> Expr -> Expr
        subst n x (V y) = if x == y then n else V y
        subst n x (L y m)
            | x == y = L x m
            | free n y = (\ z -> L z $ subst n x $ subst (V z) y m) $ head $ filter (not . free n) $ (y ++) . show <$> [0 ..]
            | otherwise = L y $ subst n x m
        subst n x (A m1 m2) = app (subst n x m1) $ subst n x m2

        free :: Expr -> [Char] -> Bool
        free (V y) x = x == y
        free (L y m) x = x /= y && free m x
        free (A m1 m2) x = free m1 x || free m2 x
app m1 m2 = A m1 m2

parse :: [Char] -> [Char]
parse = str . foldl1 app . parse' . tail . scanl (\ (_, i) x -> (x, i + fromEnum (x == '(') - fromEnum (x == ')'))) (' ', 0)
    where
        str :: Expr -> [Char]
        str (V x) = x
        str (L x m) = "\\" ++ x ++ "." ++ str m
        str (A (L x m) (V y)) = "(" ++ str (L x m) ++ ") " ++ y
        str (A (L x m) n) = "(" ++ str (L x m) ++ ") (" ++ str n ++ ")"
        str (A m (V x)) = str m ++ " " ++ x
        str (A m n) = str m ++ " (" ++ str n ++ ")"

        parse' :: [(Char, Int)] -> [Expr]
        parse' (('(', i) : t) = (\ (x, y) -> foldl1 app (parse' x) : parse' y) $ span ((>= i) . snd) t
        parse' (('\\', _) : t) = (\ (x, y) -> [L (fst <$> x) $ foldl1 app $ parse' (dropWhile ((/= '.') . fst) y)]) $ span (isAlphaNum . fst) $ dropWhile (not . isAlpha . fst) t
        parse' ((x, _) : t) = if isAlpha x then (\ (y, z) -> V (x : map fst y) : parse' z) $ span (isAlphaNum . fst) t else parse' t
        parse' _ = []

main :: IO ()
main = do
    args <- getArgs
    if null args then f else do
        x <- readFile $ head args
        putStrLn $ parse x
    where
        f :: IO ()
        f = do
            putStr "> "
            x <- getLine
            putStrLn $ parse x
            f
