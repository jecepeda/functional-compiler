module GrammarSpec
where

import Test.Hspec
import Tokens
import Grammar

grammarSpec :: IO()
grammarSpec = hspec $ do
    describe "Grammar" $ do
        it "Gets an assigment structure" $ do
            let expectedStructure = [Assign "a" (Int 1)]
            let tokens = [TokenVar, 
                          (TokenSym "a"), 
                          TokenAssign, 
                          (TokenInt 1), 
                          TokenNewLine]
            (parseTokenss tokens) `shouldBe` expectedStructure

        it "Gets an intOp structure" $ do
            let expectedStructure = [Tok (Plus 2 (Minus 2 (Int 1)))]
            let tokens = [(TokenInt 2), 
                          TokenPlus, 
                          (TokenInt 2), 
                          TokenMinus, 
                          (TokenInt 1), 
                          TokenNewLine]
            (parseTokenss tokens) `shouldBe` expectedStructure
        
        it "Gets a variable call" $ do
            let expectedStructure = [Sym "a"]
            let tokens = [(TokenSym "a"), TokenNewLine]
            (parseTokenss tokens) `shouldBe` expectedStructure

        it "Gets more than one structure" $ do
            let expectedStructure = [(Sym "a"), (Sym "b")]
            let tokens = [(TokenSym "a"), TokenNewLine, (TokenSym "b"), TokenNewLine]
            (parseTokenss tokens) `shouldBe` expectedStructure