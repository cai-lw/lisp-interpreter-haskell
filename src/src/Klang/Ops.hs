{-# LANGUAGE ScopedTypeVariables #-}

module Klang.Ops where
import Klang.Type
import Control.Applicative

liftR :: (KType a, KType b) => (a -> b) -> Res -> Res
liftR f ekx = do
    x <- ekx >>= unpack
    return $ pack (f x)

liftR2 :: (KType a, KType b, KType c) => (a -> b -> c) -> Res -> Res -> Res
liftR2 f ekx eky = do
    x <- ekx >>= unpack
    y <- eky >>= unpack
    return $ pack (f x y)

liftK :: (KType a, KType b) => (a -> b) -> KFunc1
liftK = liftA . liftR

liftK2 :: (KType a, KType b, KType c) => (a -> b -> c) -> KFunc2
liftK2 = liftA2 . liftR2

notOp :: KFunc1
notOp = liftK not

andOp :: KFunc2
andOp = liftK2 (&&)

orOp :: KFunc2
orOp = liftK2 (||)

addOp :: KFunc2
addOp = liftK2 ((+) :: Double -> Double -> Double)

subOp :: KFunc2
subOp = liftK2 ((-) :: Double -> Double -> Double)

mulOp :: KFunc2
mulOp = liftK2 ((*) :: Double -> Double -> Double)

divOp :: KFunc2
divOp = liftK2 ((/) :: Double -> Double -> Double)

eqOp :: KFunc2
eqOp = liftK2 ((==) :: Double -> Double -> Bool)

gtOp :: KFunc2
gtOp = liftK2 ((>) :: Double -> Double -> Bool)

ltOp :: KFunc2
ltOp = liftK2 ((<) :: Double -> Double -> Bool)

geOp :: KFunc2
geOp = liftK2 ((>=) :: Double -> Double -> Bool)

leOp :: KFunc2
leOp = liftK2 ((<=) :: Double -> Double -> Bool)

consOp :: KFunc2
consOp = liftK2 $ curry KPair

carOp :: KFunc1
carOp = liftK (fst :: (Data, Data) -> Data)

cdrOp :: KFunc1
cdrOp = liftK (snd :: (Data, Data) -> Data)
