{-# LANGUAGE LambdaCase #-}
import Distribution.Fields (ParseResult)
import Control.Applicative (Alternative, empty, (<|>), many, some)
import Control.Monad (void)
import Data.Function ((&))
import Data.Char (isNumber, isAlpha, isAlphaNum)
import Data.Map (Map, insert, lookup, empty, size, singleton)
import System.Environment (getArgs)
import GHC.IO.FD (openFile)
import qualified Control.Applicative as Applicative
import qualified Control.Applicative as Map
import Text.Printf
import Control.Monad.State (State)
import Data.Fixed (mod')

import Virtual (Inst(..), Program, compileProgram, compileInst)

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
    empty = Parser $ const Applicative.empty
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


-- Operator
data MultiplicativeOp = MulOp | DivOp | ModOp
    deriving (Show, Eq)
data AdditiveOp = PlusOp | MinusOp
    deriving (Show, Eq)
type Iden = String
-- Token
data Token = AddOpTok AdditiveOp | MulOpTok MultiplicativeOp | Assign | Equal | Lparen | Rparen | IntTok Int | FloatTok Float | IdenTok Iden
    deriving (Show, Eq)
type TokenParser = Parser Token
-- Value & Type
data Val = ValI Int | ValF Float
    deriving (Eq)
instance Show Val where
    show (ValI i) = show i
    show (ValF f) = show f
instance Num Val where
    (+) :: Val -> Val -> Val
    (+) (ValI x) (ValI y) = ValI (x + y)
    (+) (ValF x) (ValF y) = ValF (x + y)
    (+) (ValI x) y = ValF (fromIntegral x) + y
    (+) y (ValI x) = ValF (fromIntegral x) + y


    (*) (ValI x) (ValI y) = ValI (x * y)
    (*) (ValF x) (ValF y) = ValF (x * y)
    (*) (ValI x) y = ValF (fromIntegral x) * y
    (*) y (ValI x) = ValF (fromIntegral x) * y

    abs (ValF f) = ValF $ abs f
    abs (ValI i) = ValI $ abs i

    signum (ValF f) = ValF $ signum f
    signum (ValI i) = ValI $ signum i

    fromInteger = ValI . fromInteger
    negate (ValI i) = ValI (negate i)
    negate (ValF f) = ValF (negate f)
instance Fractional Val where
    (/) (ValI x) (ValI y) = ValI (x `div` y)
    (/) (ValF x) (ValF y) = ValF (x / y)
    (/) (ValI x) y = ValF (fromIntegral x) / y
    (/) y (ValI x) = y / ValF (fromIntegral x)

    fromRational r = ValF $ fromRational r
(%.) :: Val -> Val -> Val
(%.) (ValI x) (ValI y) = ValI (x `mod` y)
(%.) (ValF x) (ValF y) = ValF (x `mod'` y)
(%.) (ValI x) y = ValF (fromIntegral x) %. y
(%.) y (ValI x) = y %. ValF (fromIntegral x) 
-- Expression
type Expr = AddExpr
data AddExpr =  AddExpr MulExpr [(AdditiveOp, MulExpr)]
    deriving (Show, Eq)
data MulExpr = MulExpr PrimaryExpr [(MultiplicativeOp, PrimaryExpr)]
    deriving (Show, Eq)
data PrimaryExpr = ValExpr Val | VarExpr Iden | ParenExpr Expr
    deriving (Show, Eq)

data Statement = VarDefSta Iden Expr | EvalSta Expr
    deriving (Show, Eq)
ws :: StringParser ()
ws = void $ many $ charP ' ' <|> charP '\t' <|> charP '\n'

-- a = [x], b = [x] -> [x]
-- b = [x] -> [x], c = [x]
intTokP :: StringParser Token
intTokP = IntTok . read <$> some (charPredP isNumber)
idenTokP :: StringParser Token
idenTokP = IdenTok <$> ((:) <$> charPredP isAlpha <*> many (charPredP isAlphaNum) )
floatP :: StringParser Token
floatP = FloatTok . read <$> ((++) <$> some (charPredP isNumber) <*> ((:) <$> charP '.' <*> some (charPredP isNumber)))
commentP = charP
tokenP =
    ws *>
    (
        AddOpTok PlusOp <$ charP '+' <|>
        AddOpTok MinusOp <$ charP '-' <|>
        MulOpTok MulOp <$ charP '*' <|>
        MulOpTok DivOp <$ charP '/' <|>
        MulOpTok ModOp <$ charP '%' <|>
        Assign <$ stringP ":=" <|>
        Equal <$ charP '=' <|>
        Lparen <$ charP '(' <|>
        Rparen <$ charP ')' <|>
        floatP <|>
        intTokP <|>
        idenTokP
    )
-- intexprP :: TokenParser Expr
-- intexprP = Parser $ \case
--     (IntTok i):xs -> Just (xs, IntExpr i)
--     _ -> Nothing
-- binopExpr :: TokenParser Expr
-- binopExpr = Parser $ \ 
tokensP = many $ tokenP <* ws
idP :: Eq r => r -> Parser r r
idP t = Parser $ \case
    x:xs -> if t == x then Just (xs, t) else Nothing
    _ -> Nothing
pExprP :: TokenParser PrimaryExpr
mulExprP :: TokenParser MulExpr
addExprP :: TokenParser AddExpr
exprP :: TokenParser Expr
pExprP =
    Parser (\case
    (IntTok i):xs -> Just (xs, ValExpr (ValI i))
    (IdenTok s):xs -> Just (xs, VarExpr s)
    (FloatTok f):xs -> Just (xs, ValExpr (ValF f))
    _ -> Nothing) <|>
    ParenExpr <$> (idP Lparen *> exprP <* idP Rparen)

mulExprP = MulExpr <$> pExprP <*> many rest
    where
        rest :: TokenParser (MultiplicativeOp, PrimaryExpr)
        op = Parser $ \case
            (MulOpTok i):xs -> Just (xs, i)
            _ -> Nothing
        rest = (,) <$> op <*> pExprP
addExprP = AddExpr <$> mulExprP <*> many rest
    where
        rest :: TokenParser (AdditiveOp, MulExpr)
        op = Parser $ \case
            (AddOpTok i):xs -> Just (xs, i)
            _ -> Nothing
        rest = (,) <$> op <*> mulExprP
exprP = addExprP

type Closure = Map Iden Expr
evalExpr :: Expr -> Closure-> Val
evalAddExpr :: AddExpr -> Closure -> Val
evalMulExpr :: MulExpr -> Closure-> Val
evalPrimaryExpr :: PrimaryExpr -> Closure -> Val

evalExpr = evalAddExpr
evalAddExpr (AddExpr e es) c = evalList (evalMulExpr e c) es c
    where
        evalList acc [] c = acc
        evalList acc ((PlusOp,  e):es) c    = evalList (acc +   evalMulExpr e c) es c
        evalList acc ((MinusOp, e):es) c    = evalList (acc -   evalMulExpr e c) es c

evalMulExpr (MulExpr e es) c = evalList (evalPrimaryExpr e c) es c
    where
        evalList acc [] c = acc
        evalList acc ((MulOp, e):es) c      = evalList (acc *   evalPrimaryExpr e c) es c
        evalList acc ((DivOp, e):es) c      = evalList (acc /   evalPrimaryExpr e c) es c
        evalList acc ((ModOp, e):es) c      = evalList (acc %.  evalPrimaryExpr e c) es c
evalPrimaryExpr (ValExpr v) c     = v
evalPrimaryExpr (ParenExpr e) c   = evalExpr e c
evalPrimaryExpr (VarExpr name) c = evalExpr e c
    where Just e = Data.Map.lookup name c


staP :: TokenParser Statement
varDefStaP :: TokenParser Statement
evalStaP :: TokenParser Statement
varDefStaP = VarDefSta <$> (idenP <* idP Assign) <*> exprP
    where
        idenP = Parser $ \case
            (IdenTok s):xs -> Just (xs, s)
            _ -> Nothing
evalStaP = EvalSta <$> exprP <* idP Equal
staP = varDefStaP <|> evalStaP
stasP :: Parser Token [Statement]
stasP = many staP

runStas :: [Statement] -> IO ()
runStas stas = snd $ runSta' (Data.Map.empty, pure ()) stas
    where
        f :: (Closure, IO ()) -> Statement -> (Closure, IO ())
        f (c, io) (VarDefSta name expr)  = (insert name resolved c, io)
            where resolved = AddExpr (MulExpr (evalExpr expr c & ValExpr) []) []
        f (c, io) (EvalSta expr)         = (c, io >>= const (print $ evalExpr expr c))
        runSta' :: (Closure, IO ()) -> [Statement] -> (Closure, IO ())
        runSta' = Prelude.foldl f


-- 3.0 * 4
-- f [(*, 4)]

type Stack = Map String (Int, Val)

-- resolveExpr         :: (Expr, Val) -> S

compileExpr         :: Expr -> Stack -> ([Inst], Val)
compileAddExpr      :: AddExpr -> Stack -> ([Inst], Val)
compileMulExpr      :: MulExpr -> Stack -> ([Inst], Val)
compilePrimaryExpr  :: PrimaryExpr -> Stack -> ([Inst], Val)


compileExpr = compileAddExpr
compileAddExpr (AddExpr e es) stk = compileList (compileMulExpr e stk) es stk
    where
        compileList acc [] stk = acc
        compileList (insts1, v1) ((op, e):es) stk = 
            let (insts2, v2) = compileMulExpr e stk in 
            let v = v1 + v2 in 
            let insts = case (v1, v2) of
                    (ValI _, ValF _) -> insts1 ++ [Insti2f] ++ insts2
                    (ValF _, ValI _) -> insts1 ++ insts2 ++ [Insti2f]
                    _ -> insts1 ++ insts2 in
            let acc = case v of
                    ValI _ -> case op of
                        PlusOp -> (insts ++ [InstAdd], v)
                        MinusOp-> (insts ++ [InstSub], v)
                    ValF _ -> case op of
                        PlusOp -> (insts ++ [InstAddf], v)
                        MinusOp-> (insts ++ [InstSubf], v)
            in compileList acc es stk

compileMulExpr (MulExpr e es) c = compileList (compilePrimaryExpr e c) es c
    where 
        compileList acc [] stk = acc
        compileList (insts1, v1) ((op, e):es) stk = 
            let (insts2, v2) = compilePrimaryExpr e stk in 
            let v = v1 + v2 in 
            let insts = case (v1, v2) of
                    (ValI _, ValF _) -> insts1 ++ [Insti2f] ++ insts2
                    (ValF _, ValI _) -> insts1 ++ insts2 ++ [Insti2f]
                    _ -> insts1 ++ insts2 in
            let acc = case v of
                    ValI _ -> case op of
                        MulOp -> (insts ++ [InstMul], v)
                        DivOp -> (insts ++ [InstDiv], v)
                        ModOp -> (insts ++ [InstMod], v)
                    ValF _ -> case op of
                        MulOp -> (insts ++ [InstMulf], v)
                        DivOp -> (insts ++ [InstDivf], v)
                        ModOp -> (insts ++ [InstModf], v) 
            in compileList acc es stk
compilePrimaryExpr (ValExpr (ValI i)) stk        =  ([InstPush   i], ValI 0)
compilePrimaryExpr (ValExpr (ValF f)) stk        =  ([InstPushf  f], ValF 0)
compilePrimaryExpr (ParenExpr e) stk      = compileExpr e stk
compilePrimaryExpr (VarExpr name) stk     = ([InstDupBase off], v) -- TODO
    where
        Just (off, v) = Data.Map.lookup name stk

compileStas :: [Statement] -> [Inst]
compileStas stas = evals ++ [InstPop (size stk)]
    where
        (var_defs, stk) = foldl f ([], Data.Map.empty) (filter filter_var stas)
        (evals, _) = foldl f (var_defs, stk) (filter (not <$> filter_var) stas)
        filter_var sta = case sta of
            VarDefSta _ _   -> True
            _               -> False
        f :: ([Inst], Stack) -> Statement -> ([Inst], Stack)
        f  (inst1, stk) sta = (inst1 ++ inst2, stk2)
            where (inst2, stk2) = compileSta sta stk
        compileSta :: Statement -> Stack -> ([Inst], Stack)
        compileSta (VarDefSta name e) stk = (insts, insert name (count, v) stk)
            where 
                count = size stk + 1
                (insts, v) = compileExpr e stk
        compileSta (EvalSta e) stk = (insts ++ [instShow], stk)
            where 
                (insts, v) = compileExpr e stk
                instShow = case v of
                    ValI _ -> InstShow
                    ValF _ -> InstShowf

test :: IO ()
test = do
    print expr_res
    print sta_res
    where
        expr_res = do
            (_, tokens) <- run tokensP "(3 + 4) * 2 - 10 / 2"
            (_, expr) <- run exprP tokens
            return $ evalExpr expr Data.Map.empty
        sta_res = do
            (_, tokens) <- run tokensP "a := 5"
            (_, sta) <- run staP tokens
            return sta


main :: IO ()
main = do
    cal_path:out_path:_ <- getArgs
    file_content <- readFile cal_path
    let Just stas = do
            (_, tokens) <- run tokensP file_content
            (_, stas) <- run stasP tokens
            return stas
    runStas stas
    writeFile out_path $ compileProgram $ compileStas stas
    return ()









