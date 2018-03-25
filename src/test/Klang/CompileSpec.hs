module Klang.CompileSpec where
import Test.Hspec
import Klang.Parse
import Klang.Compile
import Klang.Type
import Data.Either

interpretAST :: SExp -> Res
interpretAST s = compile s >>= (\k -> run k [])

spec :: Spec
spec = do
    describe "simple tests" $ do
        it "accept correct program and run it" $ do
            interpretAST (Func "+" [Lit (KDouble 1.0), Lit (KDouble 2.0)]) `shouldBe` Right (KDouble 3.0)
            interpretAST (Func "apply" [Func "lambda" [Var "x", Lit (KBool True)], Lit (KDouble 2.0)]) `shouldBe` Right (KBool True)
        it "reject wrong number of arguments" $ do
            compile (Func "-" [Lit (KDouble 1.0)]) `shouldSatisfy` isLeft
            compile (Func "not" [Lit (KBool True), Var "x"]) `shouldSatisfy` isLeft
        it "reject non-variable on let and lambda" $ do
            compile (Func "let" [Lit (KDouble 1.0), Lit (KDouble 2.0), Lit (KDouble 3.0)]) `shouldSatisfy` isLeft
            compile (Func "lambda" [Lit (KDouble 1.0), Var "x"]) `shouldSatisfy` isLeft
        it "but accept type mismatch" $ do
            compile (Func "*" [Lit (KDouble 1.0), Lit (KBool True)]) `shouldSatisfy` isRight
            compile (Func "apply" [Lit (KDouble 1.0), Var "x"]) `shouldSatisfy` isRight
