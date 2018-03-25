{-# LANGUAGE OverloadedStrings #-}

module KiSpec where
import Test.Hspec
import Klang.Parse
import Klang.Compile
import Klang.Type
import Data.Either
import qualified Data.Text as Tx

interpret :: Tx.Text -> Res
interpret s = parseProg s >>= compile >>= (\k -> run k [])

mkList :: [Data] -> Data
mkList [] = KNil
mkList (x:xs) = KPair (x, mkList xs)

spec :: Spec
spec = do
    describe "simple tests" $ do
        it "arithmetics" $ do
            interpret "(- (* (+ 1 2) 2.5) (/ 5 2))" `shouldBe` Right (KDouble 5.0)
        it "bool and relation" $ do
            interpret "(and (or True False) (not True))" `shouldBe` Right (KBool False)
            interpret "(or (< 123 456) (>= 123 456))" `shouldBe` Right (KBool True)
        it "pair, char and string" $ do
            interpret "(cons 'c' (cdr \"bat\"))" `shouldBe` Right (mkList $ map KChar "cat")
            interpret "(cons (car \"abc\") 1)" `shouldBe` Right (KPair (KChar 'a', KDouble 1.0))
        it "let and lambda" $ do
            interpret "(let x (+ 1 2) (let y (+ 3 4) (* x y)))" `shouldBe` Right (KDouble 21.0)
            interpret "(let id (lambda x x) (id nil))" `shouldBe` Right KNil
    describe "error tests" $ do
        it "type check" $ do
            interpret "(+ 1 True)" `shouldSatisfy` isLeft
            interpret "(and 1 True)" `shouldSatisfy` isLeft
            interpret "('f' 'x')" `shouldSatisfy` isLeft
        it "absent variable" $ do
            interpret "((lambda x (f x)) 1)" `shouldSatisfy` isLeft
            interpret "(let x 3 (+ y 1))" `shouldSatisfy` isLeft
