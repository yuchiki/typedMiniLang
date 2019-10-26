module EvalSpec (spec) where

import Test.Hspec
import Expr
import Eval
import Value

import qualified Data.Map as Map

getMatcher env = (shouldBecomeTo, shouldFails)
    where
        evalWithSpecificEnv = eval env
        e `shouldBecomeTo` v = evalWithSpecificEnv e `shouldBe` Just v
        shouldFails e = evalWithSpecificEnv e `shouldBe` Nothing

notYet = return ()

spec :: Spec
spec =
    describe "eval" $ do
        context "given (EVar x)" $ do
            let (shouldBecomeToWithX, shouldFailsWithX) = getMatcher (Map.fromList [("x", VInt 1)])
            context "when the variable found" $
                it "returns the value" $
                    EVar "x" `shouldBecomeToWithX` VInt 1
            context "when the variable not found" $
                it "fails when the variable undefined" $
                    shouldFailsWithX $ EVar "y"
        context "given (ECBool b)" notYet
        context "given (ECInt i)" notYet
        context "given (EAdd e1 e2)" notYet
        context "given (ESub e1 e2)" notYet
        context "given (EMul e1 e2)" notYet
        context "given (EDiv e1 e2)" notYet
        context "given (EAnd e1 e2)" notYet
        context "given (EOr e1 e2)" notYet
        context "given (ELT e1 e2)" notYet
        context "given (EEqInt e1 e2)" notYet
        context "given (EIf e1 e2 e3)" notYet
        context "given (ELet x e1 e2)" notYet
        context "given (EAbs v e1)" notYet
        context "given (EApp e1 e2)" notYet
    where
        (shouldBecomeTo, shouldFails) = getMatcher Map.empty
