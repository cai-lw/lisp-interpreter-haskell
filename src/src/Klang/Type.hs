{-# LANGUAGE FlexibleInstances #-}
module Klang.Type where
import Data.List
import Data.Maybe
import Control.Applicative

newtype K h a = K { run :: h -> a }

instance Show (K h a) where
    show = const "<K object>"

instance Applicative (K h) where
    pure = K . const
    f <*> x =  K $ \h -> run f h (run x h)

instance Functor (K h) where
    fmap f = (<*>) (pure f)

data Data =
    KBool Bool
    | KDouble Double
    | KChar Char
    | KPair (Data, Data)
    | KNil
    | KLam (Res -> Res)

instance Eq Data where
    (KBool x) == (KBool y) = x == y
    (KDouble x) == (KDouble y) = x == y
    (KChar x) == (KChar y) = x == y
    (KPair x) == (KPair y) = x == y
    KNil == KNil = True
    _ == _ = False

typeString :: Data -> String
typeString (KBool _) = "bool"
typeString (KDouble _) = "double"
typeString (KChar _) = "char"
typeString (KPair _) = "pair"
typeString KNil = "nil"
typeString (KLam _) = "lambda"

showStr :: Data -> Maybe String
showStr KNil = Just ""
showStr (KPair (KChar c, cs)) = (c:) <$> showStr cs
showStr _ = Nothing

showLs :: Data -> Maybe String
showLs KNil = Just ""
showLs (KPair (t, KNil)) = Just $ show t
showLs (KPair (x, xs)) = ((show x ++ ", ") ++) <$> showLs xs
showLs _ = Nothing

instance Show Data where
    show (KBool b) = show b
    show (KDouble d) = show d
    show (KChar c) = show c
    show p@(KPair (l, r)) = fromJust $
        (\s -> "\"" ++ s ++ "\"") <$> showStr p
        <|> (\s -> "[" ++ s ++ "]") <$> showLs p
        <|> Just ("(" ++ show l ++ ", " ++ show r ++ ")")
    show KNil = "nil"
    show (KLam _) = "<lambda>"

type Res = Either String Data

class KType a where
    pack :: a -> Data
    unpack :: Data -> Either String a

instance KType Data where
    pack = id
    unpack = return

instance KType Bool where
    pack = KBool
    unpack (KBool b) = Right b
    unpack d = Left $ "Expect bool, found " ++ typeString d

instance KType Double where
    pack = KDouble
    unpack (KDouble x) = Right x
    unpack d = Left $ "Expect double, found " ++ typeString d

instance (KType a, KType b) => KType (a, b) where
    pack (x, y) = KPair (pack x, pack y)
    unpack (KPair (kx, ky)) = do
        x <- unpack kx
        y <- unpack ky
        return (x, y)
    unpack d = Left $ "Expect pair, found " ++ typeString d

instance KType (Res -> Res) where
    pack = KLam
    unpack (KLam f) = Right f
    unpack d = Left $ "Expect lambda, found " ++ typeString d

type Env = [(String, Res)]

type KTerm = K Env Res

type KFunc1 = KTerm -> KTerm

type KFunc2 = KTerm -> KTerm -> KTerm
