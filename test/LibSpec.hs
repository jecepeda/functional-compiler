module LibSpec
where 

import Test.Hspec
import Lib
import Grammar

libSpec :: IO()
libSpec = hspec $ do
    describe "Lib" $ do
        it "Assign a value correctly" $ do
            let expression = [(Assign "a" (Int 1))]
            let expected = [("a", 1)]
            obtained <- eval expression emptyDataStore
            obtained `shouldBe` expected

        it "Calc a value given a variable" $ do
            let expression = [(Assign "a" (Int 1)), (Assign "a" (Plus (Sym "a") (Sym "a")))]
            let expected = [("a", 2)]
            obtained <- eval expression emptyDataStore
            obtained `shouldBe` expected

        it "Does anything when undeclared variable" $ do
            let expression = [Tok (Sym "abc")]
            let expected = []
            obtained <- eval expression emptyDataStore
            obtained `shouldBe` expected

        it "Evaluate and if (condition true)" $ do
            let expression = [IfExp (If (Less (Int 3) (Int 4)) [Assign "a" (Int 3)])]
            let expected = [("a", 3)]
            obtained <- eval expression emptyDataStore
            obtained `shouldBe` expected

        it "Evaluate and if (condition false)" $ do
            let expression = [IfExp (If (Greater (Int 3) (Int 4)) [Assign "a" (Int 3)])]
            let expected = []
            obtained <- eval expression emptyDataStore
            obtained `shouldBe` expected

        it "Evaluate and ifelse (condition true)" $ do
            let expression = [IfExp (IfElse (Less (Int 3) (Int 4)) [Assign "a" (Int 3)] [Assign "a" (Int 4)])]
            let expected = [("a", 3)]
            obtained <- eval expression emptyDataStore
            obtained `shouldBe` expected

        it "Evaluate and ifelse (condition false)" $ do
            let expression = [IfExp (IfElse (Greater (Int 3) (Int 4)) [Assign "a" (Int 3)] [Assign "a" (Int 4)])]
            let expected = [("a", 4)]
            obtained <- eval expression emptyDataStore
            obtained `shouldBe` expected

        it "Print something!" $ do
            let expression = [Print (Int 2)]
            let expected = []
            obtained <- eval expression emptyDataStore
            obtained `shouldBe` expected
        
        it "While expression finished with value expected" $ do
            let expression = [Assign "a" (Int 3), WhileExp (Greater (Sym "a") (Int 0)) [Assign "a" (Minus (Sym "a") (Int 1))]]
            let expected = [("a", 0)]
            obtained <- eval expression emptyDataStore
            obtained `shouldBe` expected