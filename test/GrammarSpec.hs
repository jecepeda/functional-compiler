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
                          TokenSym "a",
                          TokenAssign,
                          TokenInt 1,
                          TokenNewLine]
            parseTokenss tokens `shouldBe` expectedStructure

        it "Gets an intOp structure" $ do
            let expectedStructure = [Tok (Plus 2 (Minus 2 (Int 1)))]
            let tokens = [TokenInt 2,
                          TokenPlus,
                          TokenInt 2,
                          TokenMinus,
                          TokenInt 1, 
                          TokenNewLine]
            parseTokenss tokens `shouldBe` expectedStructure

        it "Gets a variable call" $ do
            let expectedStructure = [Sym "a"]
            let tokens = [TokenSym "a", TokenNewLine]
            parseTokenss tokens `shouldBe` expectedStructure

        it "Gets more than one structure" $ do
            let expectedStructure = [Sym "a", Sym "b"]
            let tokens = [TokenSym "a", TokenNewLine, TokenSym "b", TokenNewLine]
            parseTokenss tokens `shouldBe` expectedStructure
        
        it "Gets an if conditional" $ do
            let expectedStructure = [Tok 1, Tok 2, Assign "a" (Int 1),Assign "a" (Int 2)]
            let tokens = [TokenIf,
                         TokenInt 1,
                         TokenLess,
                         TokenInt 2,
                         TokenNewLine,
                         TokenVar,
                         TokenSym "a",
                         TokenAssign,
                         TokenInt 1,
                         TokenNewLine,
                         TokenElse,
                         TokenNewLine,
                         TokenVar,
                         TokenSym "a",
                         TokenAssign,
                         TokenInt 2,
                         TokenNewLine
                         ]
            parseTokenss tokens `shouldBe` expectedStructure
        
        it "Gets a while conditional" $ do
            let expectedStructure = [Tok 1, Tok 2, Tok (Plus 2 (Minus 2 (Int 1)))]
            let tokens = [TokenWhile,
                         TokenInt 1,
                         TokenLess,
                         TokenInt 2,
                         TokenNewLine,
                         TokenInt 2,
                         TokenPlus,
                         TokenInt 2,
                         TokenMinus,
                         TokenInt 1, 
                         TokenNewLine,
                         TokenEndWhile
                         ]
            parseTokenss tokens `shouldBe` expectedStructure
        
        it "Gets a print" $ do
            let expectedStructure = [Sym "HelloWorld"]
            let tokens = [TokenPrint, TokenSym "HelloWorld", TokenNewLine]
            parseTokenss tokens `shouldBe` expectedStructure
            