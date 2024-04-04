{-# LANGUAGE LambdaCase #-}
import Foreign.C (CChar)
import Distribution.Fields (ParseResult)
import Control.Applicative (Alternative, empty, (<|>), many, some)
import Control.Monad (void)
import Data.Char (isNumber)
import Distribution.Compat.CharParsing (CharParsing(string))
import Text.Parsec.Token (GenTokenParser(TokenParser))
newtype Parser i r = Parser { run :: [i] -> Maybe ([i], r)}
instance Functor (Parser i) where
    fmap :: (r1 -> r2) -> Parser i r1 -> Parser i r2
    fmap f (Parser p) = Parser $ \input -> do
        (input', r) <- p input
        return (input', f r)
instance Applicative (Parser i) where
    pure :: a -> Parser i a
    pure a = Parser (\input -> Just (input, a))
    (<*>) :: Parser i (a -> b) -> Parser i a -> Parser i b
    (Parser f) <*> (Parser a) = Parser $ \input -> do
        (input', ff) <- f input
        (input'', aa) <- a input'
        return (input'', ff aa)
instance Alternative (Parser i) where
    empty = Parser $ const empty
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input
type StringParser = Parser Char
charPredP :: (Char -> Bool) -> StringParser Char
charPredP pred = Parser (\case
        x:xs -> if pred x then Just (xs, x) else Nothing
        _ -> Nothing)
stringPredP :: (Char -> Bool) -> StringParser String
stringPredP pred = many $ charPredP pred
charP :: Char -> StringParser Char
charP c = charPredP (==c)
stringP :: String -> StringParser String
stringP = traverse charP

data MultiplicativeOp = MulOp | DivOp
    deriving (Show, Eq)
data AdditiveOp = PlusOp | MinusOp
    deriving (Show, Eq)
data Token = AddOpTok AdditiveOp | MulOpTok MultiplicativeOp | Lparen | Rparen | IntTok Int
    deriving (Show, Eq)
type TokenParser = Parser Token

type Expr = AddExpr
newtype AddExpr =  AddExpr (MulExpr, [(AdditiveOp, MulExpr)])
    deriving (Show, Eq)
newtype MulExpr = MulExpr (PrimaryExpr, [(MultiplicativeOp, PrimaryExpr)])
    deriving (Show, Eq)
data PrimaryExpr = IntExpr Int | ParenExpr Expr
    deriving (Show, Eq)
ws :: StringParser ()
ws = void $ many $ charP ' ' <|> charP '\t' <|> charP '\n'


inttokP :: StringParser Token
inttokP = IntTok . read <$> some (charPredP isNumber)


tokenP =
    ws *>
    (
        AddOpTok PlusOp <$ charP '+' <|>
        AddOpTok MinusOp <$ charP '-' <|>
        MulOpTok MulOp <$ charP '*' <|>
        MulOpTok DivOp <$ charP '/' <|>
        Lparen <$ charP '(' <|>
        Rparen <$ charP ')' <|>
        inttokP
    )
-- intexprP :: TokenParser Expr
-- intexprP = Parser $ \case
--     (IntTok i):xs -> Just (xs, IntExpr i)
--     _ -> Nothing
-- binopExpr :: TokenParser Expr
-- binopExpr = Parser $ \ 
tokensP = many $ tokenP <* ws
singleTokP :: Eq a => a -> Parser a ()
singleTokP t = Parser $ \case
    x:xs -> if t == x then Just (xs, ()) else Nothing
    _ -> Nothing
pExprP :: TokenParser PrimaryExpr
mulExprP :: TokenParser MulExpr
addExprP :: TokenParser AddExpr
exprP :: TokenParser Expr
pExprP = Parser (\case
    (IntTok i):xs -> Just (xs, IntExpr i)
    _ -> Nothing) <|> ParenExpr <$> (singleTokP Lparen *> exprP <* singleTokP Rparen)

mulExprP =  MulExpr <$> ((,) <$> pExprP <*> many rest)
    where
        rest :: TokenParser (MultiplicativeOp, PrimaryExpr)
        op = Parser $ \case
            (MulOpTok i):xs -> Just (xs, i)
            _ -> Nothing
        rest = (,) <$> op <*> pExprP
addExprP = AddExpr <$> ((,) <$> mulExprP <*> many rest)
    where
        rest :: TokenParser (AdditiveOp, MulExpr)
        op = Parser $ \case
            (AddOpTok i):xs -> Just (xs, i)
            _ -> Nothing
        rest = (,) <$> op <*> mulExprP
exprP = addExprP


evalExpr :: Expr -> Float
evalAddExpr :: AddExpr -> Float
evalMulExpr :: MulExpr -> Float
evalPrimaryExpr :: PrimaryExpr -> Float

evalExpr = evalAddExpr
evalAddExpr (AddExpr (e, es)) = evalList (evalMulExpr       e)  es
    where
        evalList acc [] = acc
        evalList acc ((PlusOp, e):es)   = evalList (acc + evalMulExpr e) es
        evalList acc ((MinusOp, e):es)  = evalList (acc - evalMulExpr e) es 

evalMulExpr (MulExpr (e, es)) = evalList (evalPrimaryExpr   e)  es
    where
        evalList acc [] = acc
        evalList acc ((MulOp, e):es)    = evalList (acc * evalPrimaryExpr e) es
        evalList acc ((DivOp, e):es)    = evalList (acc / evalPrimaryExpr e) es 
evalPrimaryExpr (IntExpr i)     = fromIntegral i
evalPrimaryExpr (ParenExpr e)   = evalExpr e



main :: IO ()
main = do
    print res
    where res = do
            (_, tokens) <- run tokensP "(3 + 4) * 2 - 10 / 2"
            (_, expr) <- run exprP tokens
            return $ evalExpr expr





