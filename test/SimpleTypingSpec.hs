module SimpleTypingSpec (spec) where

import Test.Hspec

import SimpleType
import SimpleTyping
import Expr


spec :: Spec
spec = do
    inferSpec
    extractSpec
    unifySpec
    ftvSpec
    substituteOneSpec
    substituteSpec
    substituteEqsSpec

inferSpec :: Spec -- 散発的なテストにとどめて、全体の仕組みが動いているかどうかのチェックだけをする。
inferSpec =
    describe "infer" $ do
        context "given typable expressions" $ do
            it "types 1" $
                EInt 1 `hasType` TInt
            it "types 1 + 2" $
                EAdd (EInt 1) (EInt 2) `hasType` TInt
            it "types (fun x -> x + 1)" $
                EAbs "x" (EAdd (EVar "x") (EInt 1)) `hasType` TFun TInt TInt
            it "types (fun x -> x + 1) 1" $
                EApp (EAbs "x" (EAdd (EVar "x") (EInt 1))) (EInt 1) `hasType` TInt
        context "given untypable expressions" $ do
            it "rejects x" $
                hasNoType $ EVar "x"
            it "rejects 1 + true" $
                hasNoType $ EAdd (EInt 1) (EBool True)
            it "rejects (fun x -> x + 1) true" $
                hasNoType $ EApp (EAbs "x" (EAdd (EVar "x") (EInt 1))) (EBool True)



hasType :: Expr -> SimpleType -> Expectation
e `hasType` t = infer e `shouldBe` Just t

hasNoType :: Expr -> Expectation
hasNoType e = infer e `shouldBe` Nothing

extractSpec :: Spec
extractSpec =
    describe "extract" $ do
        context "given EVar" notYet
        context "given EBool" notYet
        context "given EInt" notYet
        context "given EAdd" notYet
        context "given ESub" notYet
        context "given EMul" notYet
        context "given EDiv" notYet
        context "given EAnd" notYet
        context "given EOr" notYet
        context "given ELT" notYet
        context "given EEqInt" notYet
        context "given ELet" notYet
        context "given EAbs" notYet
        context "given EApp" notYet
        context "given ENil" notYet
        context "given ECons" notYet
        context "given EMatch"  notYet

unifySpec :: Spec
unifySpec = notYet

ftvSpec :: Spec
ftvSpec = notYet

substituteOneSpec :: Spec
substituteOneSpec = notYet

substituteSpec :: Spec
substituteSpec = notYet

substituteEqsSpec :: Spec
substituteEqsSpec = notYet

notYet = it "has not yet tested" pending
