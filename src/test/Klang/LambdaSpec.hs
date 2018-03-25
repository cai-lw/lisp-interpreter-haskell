module Klang.LambdaSpec where
import Test.Hspec
import Klang.Lambda
import Klang.Ops
import Klang.Type
import Data.Either
import Data.Maybe
import System.Timeout
import Control.Exception

runTerm :: KTerm -> Res
runTerm t = run t []

applyMany :: Int -> a -> (a -> a) -> a
applyMany 0 x _ = x
applyMany k x f = f (applyMany (k-1) x f)

spec :: Spec
spec = do
    let inc = lambda "x" (addOp (lit $ KDouble 1.0) (slot "x"))
    describe "simple tests" $ do
        it "basic operations" $ do
            runTerm (apply inc (lit $ KDouble 1.0)) `shouldBe` Right (KDouble 2.0)
            runTerm (applyMany 1000 (lit $ KDouble 0.0) (apply inc)) `shouldBe` Right (KDouble 1000.0)
        it "cannot apply non-lambda term" $
            runTerm (apply (apply inc (lit $ KDouble 1.0)) (lit $ KDouble 2.0)) `shouldSatisfy` isLeft
        it "no free variable allowed when evaluated" $ do
            let free = lambda "x" (addOp (slot "y") (slot "x"))
            runTerm (apply free (lit $ KDouble 1.0)) `shouldSatisfy` isLeft
        it "but evaluate to lambda is okay" $
            runTerm inc `shouldSatisfy` isRight
    describe "combinator tests" $ do
        it "identities in SKI combinator calculus" $ do
            -- K = \x.\y.x
            let k = lambda "x" (lambda "y" (slot "x"))
            -- S = \x.\y.\z.(xz)(yz)
            let s = lambda "x" (lambda "y" (lambda "z" (apply (apply (slot "x") (slot "z")) (apply (slot "y") (slot "z")))))
            -- I = SKK = SKS
            let i1 = apply (apply s k) k
            let i2 = apply (apply s k) s
            runTerm (apply i1 (lit KNil)) `shouldBe` Right KNil
            runTerm (apply i2 (lit KNil)) `shouldBe` Right KNil
        it "omega combinator should timeout (>0.1s)" $ do
            -- Ω = ωω = (\x.xx)(\x.xx)
            let omega = lambda "x" (apply (slot "x") (slot "x"))
            res <- timeout 100000 $ evaluate (runTerm $ apply omega omega)
            res `shouldSatisfy` isNothing
        it "Y combinator is okay with const function but timeout with others" $ do
            -- Y = \f.(\x.f(xx))(\x.f(xx))
            -- Yf = f(Yf), Ω = YI
            let yPart = lambda "x" (apply (slot "f") (apply (slot "x") (slot "x")))
            let y = lambda "f" (apply yPart yPart)
            runTerm (apply y (lambda "x" (lit KNil))) `shouldBe` Right KNil
            res <- timeout 100000 $ evaluate (runTerm $ apply y inc)
            res `shouldSatisfy` isNothing
