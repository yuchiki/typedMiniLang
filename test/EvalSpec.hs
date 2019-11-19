module EvalSpec (spec) where

import Test.Hspec
import Expr
import Eval
import Value

import qualified Data.Map as Map

type OkMatcher = Expr -> Value -> Expectation
type FailMatcher = Expr -> Expectation

spec :: Spec
spec =
    describe "eval" $ do
        context "given (EVar x)" $ do
            let (shouldBecomeWithX, shouldFailWithX) = getMatcher (Map.fromList [("x", VInt 1)])
            context "when the variable found" $
                it "returns the value" $
                    EVar "x" `shouldBecomeWithX` VInt 1
            context "when the variable not found" $
                it "fails" $
                    shouldFailWithX $ EVar "y"
        context "given (EBool b)" $ do
            context "when True" $
                it "returns True" $
                    EBool True `shouldBecome` VBool True
            context "when False" $
                it "returns False" $
                    EBool False `shouldBecome` VBool False
        context "given (EInt i)" $
            it "returns i" $
                EInt 1 `shouldBecome` VInt  1
        context "given (EAdd e1 e2)" $ intOpExpectation EAdd 10 2 12
        context "given (ESub e1 e2)" $ intOpExpectation ESub 10 2 8
        context "given (EMul e1 e2)" $ intOpExpectation EMul 10 2 20
        context "given (EDiv e1 e2)" $ intOpExpectation EDiv 10 2 5
        context "given (EAnd e1 e2)" $ boolOpExpectation EAnd False False False True
        context "given (EOr e1 e2)" $ boolOpExpectation EOr False True True True
        context "given (ELT e1 e2)" $ comparatorExpectation ELT True False False
        context "given (EEqInt e1 e2)" $ comparatorExpectation EEqInt False True False
        context "given (EIf e1 e2 e3)" $ do
            context "if the condition True" $
                it "returns the first value" $
                    EIf (EBool True) (EInt 1) (EInt 2) `shouldBecome` VInt 1
            context "if the condition False" $
                it "returns the second value" $
                    EIf (EBool False) (EInt 1) (EInt 2) `shouldBecome` VInt 2
            context "if the condition is not bool" $
                it "fails" $
                    shouldFail $ EIf (EInt 1) (EInt 1) (EInt 2)
        context "given (ELet x e1 e2)" $
                it "allow to use x in e2" $
                    ELet "x" (EInt 1) (EVar "x") `shouldBecome` VInt 1
        context "given (EAbs v e1)" $
            it "returns an abstraction" $
                EAbs "x" (EBool True) `shouldBecome` VAbs "x" (EBool True)
        context "given (EApp e1 e2)" $ do
            context "when the first argument is an abstraction" $ do
                it "returns the ans" $
                    EApp (EAbs "x" (EInt 10)) (EInt 1) `shouldBecome` VInt 10
                it "allow to use the value of the 2nd argument in the 1st argument" $
                    EApp (EAbs "x" (EVar "x")) (EInt 1) `shouldBecome` VInt 1
            context "when the first argument is not an abstraction" $
                it "fails" $
                    shouldFail $ EApp (EInt 1) (EInt 2)
        context "given ENil" $
            it "returns VNil" $
                ENil `shouldBecome` VNil
        context "given (ECons e1 e2)" $
            it "returns VCons" $
                ECons (EInt 1) (EInt 2) `shouldBecome` VCons (VInt 1) (VInt 2)
        context "given (EMatch e1 e2 x1 x2 e3)" $ do
            context "when e1 evaluates to nil" $
                it "returns the value of e2" $
                    EMatch ENil (EInt 1) "x" "y" (EInt 2) `shouldBecome` VInt 1
            context "when e1 evaluates to cons" $ do
                it "returns the value of e3" $
                    EMatch (ECons (EInt 10) (EInt 2)) (EInt 1) "x" "y" (EInt 2) `shouldBecome` VInt 2
                it "binds x1 :: x2 to the value of e1 in e3" $
                    EMatch (ECons (EInt 10) (EInt 2)) (EInt 1) "x" "y" (EDiv (EVar "x") (EVar "y")) `shouldBecome` VInt 5
    where
        (shouldBecome, shouldFail) = getMatcher Map.empty
        intOpExpectation = genIntOpExpectation shouldBecome shouldFail
        boolOpExpectation = genBoolOpExpectation shouldBecome shouldFail
        comparatorExpectation = genComparatorExpectation shouldBecome shouldFail


getMatcher :: Env -> (OkMatcher, FailMatcher)
getMatcher env = (shouldBecome, shouldFail)
    where
        evalWithSpecificEnv = eval' env
        e `shouldBecome` v = evalWithSpecificEnv e `shouldBe` Just v
        shouldFail e = evalWithSpecificEnv e `shouldBe` Nothing

genIntOpExpectation :: HasCallStack => OkMatcher -> FailMatcher -> (Expr -> Expr -> Expr) -> Int -> Int -> Int -> SpecWith ()
genIntOpExpectation shouldBecome shouldFail eOp left right expected = do
    context "when both args are int" $
        it "returns the ans" $
            eOp (EInt left) (EInt right) `shouldBecome` VInt expected
    context "when the left is not int" $
        it "fails" $
            shouldFail $ eOp (EBool True) (EInt right)
    context "when the right is not int" $
        it "fails" $
            shouldFail $ eOp (EInt left) (EBool True)
    context "when the both are not int" $
        it "fails" $
            shouldFail $ eOp (EBool True) (EBool True)

genBoolOpExpectation :: HasCallStack => OkMatcher -> FailMatcher -> (Expr -> Expr -> Expr) -> Bool -> Bool -> Bool -> Bool -> SpecWith ()
genBoolOpExpectation shouldBecome shouldFail eOp b1 b2 b3 b4 = do
    context "when (False, False)" $
        it ("returns the " ++ show b1) $
            eOp (EBool False) (EBool False) `shouldBecome` VBool b1
    context "when (False, True)" $
        it ("returns the " ++ show b2) $
            eOp (EBool False) (EBool True) `shouldBecome` VBool b2
    context "when (True, False)" $
        it ("returns the " ++ show b3) $
            eOp (EBool True) (EBool False) `shouldBecome` VBool b3
    context "when (True True)" $
        it ("returns the " ++ show b4) $
            eOp (EBool True) (EBool True) `shouldBecome` VBool b4
    context "when the left is not bool" $
        it "fails" $
            shouldFail $ eOp (EInt 1) (EBool True)
    context "when the right is not bool" $
        it "fails" $
            shouldFail $ eOp (EBool True) (EInt 1)
    context "when the both are not bool" $
        it "fails" $
            shouldFail $ eOp (EInt 1) (EInt 1)

genComparatorExpectation :: HasCallStack => OkMatcher -> FailMatcher -> (Expr -> Expr -> Expr) -> Bool -> Bool -> Bool -> SpecWith ()
genComparatorExpectation shouldBecome shouldFail eOp lt eq gt = do
    context "when the left is less than the right" $
        it ("returns " ++ show lt) $
            eOp (EInt 1) (EInt 2) `shouldBecome` VBool lt
    context "when the left is equal to the right" $
        it ("returns " ++ show eq) $
            eOp (EInt 1) (EInt 1) `shouldBecome` VBool eq
    context "when the left is greater than the right" $
        it ("returns " ++ show gt) $
            eOp (EInt 2) (EInt 1) `shouldBecome` VBool gt
    context "when the left is not int" $
        it "fails" $
            shouldFail $ eOp (EBool True) (EInt 1)
    context "when the right is not int" $
        it "fails" $
            shouldFail $ eOp (EInt 1) (EBool True)
    context "when the both are not int" $
        it "fails" $
            shouldFail $ eOp (EBool True) (EBool True)
