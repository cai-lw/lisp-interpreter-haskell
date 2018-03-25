module Klang.Compile where
import Data.List
import Data.Maybe
import Klang.Parse
import Klang.Lambda
import Klang.Type
import Klang.Config

compileF1 :: (String, KFunc1) -> [SExp] -> Either String KTerm
compileF1 (_, f) [sx] = do
    x <- compile sx
    return $ f x
compileF1 (fn, _) _   = Left $ "\"" ++ fn ++ "\" expects 1 argument"

compileF2 :: (String, KFunc2) -> [SExp] -> Either String KTerm
compileF2 (_, f) [sx, sy] = do
    x <- compile sx
    y <- compile sy
    return $ f x y
compileF2 (fn, _) _   = Left $ "\"" ++ fn ++ "\" expects 2 arguments"

compileLambda :: [SExp] -> Either String KTerm
compileLambda [Var s, x] = lambda s <$> compile x
compileLambda _ = Left "Wrong syntax. Should be (lambda var expr)"

compileLet :: [SExp] -> Either String KTerm
compileLet [Var s, sx, sy] = do
    x <- compile sx
    y <- compile sy
    return $ apply (lambda s y) x
compileLet _ = Left "Wrong syntax. Should be (let var expr expr)"

compile :: SExp -> Either String KTerm
compile (Lit x) = Right $ lit x
compile (Var s) = Right $ slot s
compile (Func f args)
    | f == "let" = compileLet args
    | f == "lambda" = compileLambda args
    | f == "apply" = compileF2 ("apply", apply) args
    | isJust found1 = compileF1 (fromJust found1) args
    | isJust found2 = compileF2 (fromJust found2) args
    | otherwise = error $ "\"" ++ f ++ "\" is not a primitive function"
    where found1 = find ((== f) . fst) kFunc1
          found2 = find ((== f) . fst) kFunc2
