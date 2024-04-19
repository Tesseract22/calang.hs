{-# LANGUAGE LambdaCase #-}
import Distribution.Fields (ParseResult)
import Control.Applicative (Alternative, empty, (<|>), many, some)
import Control.Monad (void)
import Data.Function ((&))
import Data.Char (isNumber, isAlpha, isAlphaNum)
import Data.Map (Map, insert, lookup, empty, size, singleton, foldlWithKey, toList)
import Data.List (sortOn)
import System.Environment (getArgs)
import GHC.IO.FD (openFile)
import qualified Control.Applicative as Applicative
import qualified Control.Applicative as Map
import Text.Printf
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
emptiable :: Parser i [r] -> Parser i [r]
emptiable p = Parser $ \input -> case run p input of
    Just x -> Just x
    Nothing -> Just (input, [])


-- Operator
data Op = PlusOp | MinusOp | MulOp | DivOp | ModOp
    deriving (Show, Eq)
type Iden = String
data Fn = Fn [Iden] Expr Stack
    deriving (Show, Eq)

-- Token
data Token =
    OpTok Op |
    Assign | Equal |
    Lcurly | Rcurly |
    Lparen | Rparen |
    Comma |
    FnTok |
    RetTok |
    IntTok Int |
    FloatTok Float |
    IdenTok Iden
    deriving (Show, Eq)
type TokenParser = Parser Token
-- Value & Type
data Val = ValI Int | ValF Float | ValFn Fn
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
data Expr =
    PrimaryExpr PrimaryExpr |
    BinOpExpr Expr Op Expr
    deriving (Show, Eq)
-- instance Show Expr where
--     show e = "\n" ++ show' 0 e
--         where
--             show' iden (PrimaryExpr e)  = show e
--             show' iden (BinOpExpr e1 op e2)    = 
--                 replicate iden '\t' ++show' (iden + 1) e1 ++ "\n" ++ 
--                 replicate iden '\t' ++ show op ++ "\n" ++
--                 replicate iden '\t' ++ show' (iden + 1) e2 ++ "\n"
-- -- -- data AddExpr =  AddExpr MulExpr [(AdditiveOp, MulExpr)]
--     deriving (Show, Eq)
-- data MulExpr = MulExpr PrimaryExpr [(MultiplicativeOp, PrimaryExpr)]
--     deriving (Show, Eq)
data PrimaryExpr = FnAppExpr Iden [Expr] | BlkExpr [Statement] Expr | ValExpr Val | VarExpr Iden | ParenExpr Expr
    deriving (Show, Eq)

data Statement = VarDefSta Iden Expr | EvalSta Expr
    deriving (Show, Eq)
ws :: StringParser ()
ws = void $ many $ charP ' ' <|> charP '\t' <|> charP '\n'

tupleLike :: TokenParser a -> TokenParser [a]
tupleLike elP = paramsP
    where
        sepByP = idP Comma
        paramsP = idP Lparen *> emptiable ((:) <$> elP <*> many (sepByP *> elP)) <* idP Rparen
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
        OpTok PlusOp <$ charP '+' <|>
        OpTok MinusOp <$ charP '-' <|>
        OpTok MulOp <$ charP '*' <|>
        OpTok DivOp <$ charP '/' <|>
        OpTok ModOp <$ charP '%' <|>
        Assign <$ stringP ":=" <|>
        Equal <$ charP '=' <|>
        Lparen <$ charP '(' <|>
        Rparen <$ charP ')' <|>
        Lcurly <$ charP '{' <|>
        Rcurly <$ charP '}' <|>
        Comma <$ charP ',' <|>
        FnTok <$ stringP "fn" <|>
        RetTok <$ stringP "ret" <|>
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
mulExprP :: TokenParser Expr
addExprP :: TokenParser Expr
exprP :: TokenParser Expr
pExprP =
    (ValExpr . ValFn <$> (Fn <$> (idP FnTok *> tupleLike idenFromTokP) <*> exprP <*> pure Data.Map.empty)) <|>
    FnAppExpr <$> idenFromTokP <*> tupleLike exprP <|>
    BlkExpr <$> (idP Lcurly *> stasP) <*> (idP RetTok *> exprP <* idP Rcurly) <|>
    Parser (\case
    (IntTok i):xs -> Just (xs, ValExpr (ValI i))
    (IdenTok s):xs -> Just (xs, VarExpr s)
    (FloatTok f):xs -> Just (xs, ValExpr (ValF f))
    _ -> Nothing) <|>
    ParenExpr <$> (idP Lparen *> exprP <* idP Rparen)
opP :: Op -> TokenParser Op
opP o = Parser $ \case
    (OpTok o'):xs -> if o == o' then Just (xs, o) else Nothing
    _ -> Nothing
list2tree :: Expr -> [(Op, Expr)] -> Expr
list2tree= foldl f
    where
        f e1 (op, e2)= BinOpExpr e1 op e2

mulExprP = (list2tree . PrimaryExpr <$> pExprP) <*> mulExprP'
    where
        mulExprP'= many $ (,) <$> mulOpP <*> (PrimaryExpr <$> pExprP)
        mulOpP = opP MulOp <|> opP DivOp <|> opP ModOp


addExprP = list2tree <$> mulExprP <*> addExprP'
    where
        addExprP'= many $ (,) <$> addOpP <*> mulExprP
        addOpP = opP PlusOp <|> opP MinusOp
-- add = mul | add op mul
exprP = addExprP
type Stack = Map Iden (Int, Val)
evalExpr :: Expr -> Stack-> Val
evalPrimaryExpr :: PrimaryExpr -> Stack -> Val
evalOp op = case op of
    PlusOp  -> (+)
    MinusOp -> (-)
    MulOp   -> (*)
    DivOp   -> (/)
    ModOp   -> (%.)
evalExpr (PrimaryExpr pe) stk = evalPrimaryExpr pe stk
evalExpr (BinOpExpr e1 op e2) stk = evalOp op (evalExpr e1 stk) (evalExpr e2 stk)
evalPrimaryExpr (ValExpr v) c     = v
evalPrimaryExpr (ParenExpr e) c   = evalExpr e c
evalPrimaryExpr (VarExpr name) c = v
    where Just (_, v) = Data.Map.lookup name c

evalPrimaryExpr (FnAppExpr name args) c = evalExpr e fc'
    where
        evaledArgs = (`evalExpr` c) <$> args
        fc' = foldl (\acc (arg, argname) -> insert argname (0, arg) acc) fc (zip evaledArgs argsName)
        Just (_, ValFn (Fn argsName e fc)) = Data.Map.lookup name c
evalPrimaryExpr (BlkExpr stas e) c = evalExpr e c'
    where (c', _) = runStas' (c, pure ()) stas
idenFromTokP = Parser $ \case
            (IdenTok s):xs -> Just (xs, s)
            _ -> Nothing
staP :: TokenParser Statement
varDefStaP :: TokenParser Statement
evalStaP :: TokenParser Statement
varDefStaP = VarDefSta <$> (idenFromTokP <* idP Assign) <*> exprP
evalStaP = EvalSta <$> exprP <* idP Equal
staP = varDefStaP <|> evalStaP
stasP :: Parser Token [Statement]
stasP = many staP

runStas :: [Statement] -> IO ()
runStas stas = snd $ runStas' (Data.Map.empty, pure ()) stas
runStas' :: (Stack, IO ()) -> [Statement] -> (Stack, IO ())
runStas' = Prelude.foldl f
    where
        f :: (Stack, IO ()) -> Statement -> (Stack, IO ())
        f (c, io) (VarDefSta name expr)  = (insert name (0, resolved) c, io)
            where
                resolved = case evalExpr expr c of
                    ValFn (Fn args e _) -> ValFn (Fn args e c)
                    v -> v

        f (c, io) (EvalSta expr)         = (c, io >>= const (print $ evalExpr expr c))
-- 3.0 * 4
-- f [(*, 4)]



-- resolveExpr         :: (Expr, Val) -> S

compileExpr         :: Expr -> Stack -> ([Inst], Val)
compileExpr (PrimaryExpr pe) stk = compilePrimaryExpr pe stk
compileExpr (BinOpExpr e1 op e2) stk = (i ++ [iop], v)
    where
        (i1, v1) = compileExpr e1 stk
        (i2, v2) = compileExpr e2 stk
        v = v1 + v2
        i = case (v1, v2) of
            (ValI _, ValI _)    -> i1 ++ i2
            (ValF _, ValF _)    -> i1 ++ i2
            (ValF _, _)         -> i1 ++ i2 ++ [Insti2f]
            (_, ValF _)         -> i1 ++ [Insti2f] ++ i2
        iop = case v of
            ValI _ -> case op of
                PlusOp  -> InstAdd
                MinusOp -> InstSub
                MulOp   -> InstMul
                DivOp   -> InstDiv
                ModOp   -> InstMod
            ValF _ -> case op of
                PlusOp  -> InstAddf
                MinusOp -> InstSubf
                MulOp   -> InstMulf
                DivOp   -> InstDivf
                ModOp   -> InstModf
compilePrimaryExpr :: PrimaryExpr -> Stack -> ([Inst], Val)
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
                count = case v of
                    ValFn _ -> 2 * size stk
                    _       -> 1 + size stk
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
    print stas
    runStas stas
    writeFile out_path $ compileProgram $ compileStas stas
    return ()









