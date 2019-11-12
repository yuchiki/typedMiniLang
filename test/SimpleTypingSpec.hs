module SimpleTypingSpec (spec) where

import Test.Hspec


spec :: Spec
spec =
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

notYet = it "has not yet tested" pending
