{-# LANGUAGE OverloadedStrings #-}

module Klang.ParseSpec where
import Test.Hspec
import Klang.Parse
import Klang.Type
import Data.Either
import qualified Data.Text as Tx

smallPretty :: Tx.Text
smallPretty = "(+\n\
             \  (*\n\
             \    1.5\n\
             \    x\n\
             \  )\n\
             \  100\n\
             \)"

smallAST :: SExp
smallAST = Func "+" [Func "*" [Lit (KDouble 1.5), Var "x"], Lit (KDouble 100.0)]

spec :: Spec
spec = do
    describe "simple tests" $ do
        it "basic arithmetic" $ do
            parseProg "(+ (* 1.5 x) 100)" `shouldBe` Right smallAST
            parseProg "(/ (- var 1) 2)" `shouldBe` Right (Func "/" [Func "-" [Var "var", Lit (KDouble 1.0)], Lit (KDouble 2.0)])
            parseProg "(<= 2 3)" `shouldBe` Right (Func "<=" [Lit (KDouble 2.0), Lit (KDouble 3.0)])
        it "deal with whitespaces" $ do
            parseProg "(+ \t( * 1.5    x) \n100  )" `shouldBe` Right smallAST
            parseProg smallPretty `shouldBe` Right smallAST
        it "bool primitives" $
            parseProg "(and (not False) True)" `shouldBe` Right (Func "and" [Func "not" [Lit (KBool False)], Lit (KBool True)])
        it "pair, char and string" $ do
            parseProg "(cons '\\'' nil)" `shouldBe` Right (Func "cons" [Lit (KChar '\''), Lit KNil])
            parseProg "(car \"\\\"a\")" `shouldBe` Right (Func "car" [Lit (KPair (KChar '"', KPair (KChar 'a', KNil)))])
        it "functional" $ do
            parseProg "(let x 3 (> x 2))" `shouldBe` Right (Func "let" [Var "x", Lit (KDouble 3.0), Func ">" [Var "x", Lit (KDouble 2.0)]])
            parseProg "((lambda x (= x 3)) 3)" `shouldBe` Right (Func "apply" [Func "lambda" [Var "x", Func "=" [Var "x", Lit (KDouble 3.0)]], Lit (KDouble 3.0)])
            parseProg "(f 233)" `shouldBe` Right (Func "apply" [Var "f", Lit (KDouble 233.0)])
    describe "error tests" $ do
        it "unbalanced brackets and quotes" $ do
            parseProg "(+ (- x 3) (* 4 y)" `shouldSatisfy` isLeft
            parseProg "(cons 'a nil)" `shouldSatisfy` isLeft
            parseProg "(car abcd\")" `shouldSatisfy` isLeft
        it "malformed S-Expression" $ do
            parseProg "(   )" `shouldSatisfy` isLeft
            parseProg "(abc)" `shouldSatisfy` isLeft
            parseProg "(or a b) c" `shouldSatisfy` isLeft
        it "illegal characters" $ do
            parseProg "(! True)" `shouldSatisfy` isLeft
            parseProg "(☆ ★)" `shouldSatisfy` isLeft -- But Unicode scripts (汉字, かな, etc.) are allowed
        it "but wrong use of function should pass parsing" $ do
            parseProg "(+ 1 2 3 4)" `shouldSatisfy` isRight
            parseProg "(>= nil 'a')" `shouldSatisfy` isRight
            parseProg "(london bridge falling down)" `shouldSatisfy` isRight
