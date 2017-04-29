module LexerSpec
where

import Test.Hspec
import Tokens

lexerSpec :: IO()
lexerSpec = hspec $ do
    describe "Lexer" $ do
        it "Returns an integer token" $ do
            let expectedTokens = [TokenInt 123]
            scanTokens "123" `shouldBe` expectedTokens

        it "Returns a var and a string" $ do
            let expectedTokens = [TokenVar, TokenSym "helloWorld"]
            scanTokens "var helloWorld" `shouldBe` expectedTokens

        it "Returns only an new line token given enter" $ do
            let expectedTokens = [TokenNewLine]
            scanTokens "\n" `shouldBe` expectedTokens

        it "Returns only a new line token given semicolon" $ do
            let expectedTokens = [TokenNewLine]
            scanTokens ";" `shouldBe` expectedTokens

        it "Returns a plus and a minus token" $ do
            let expectedTokens = [TokenPlus, TokenMinus]
            scanTokens "+-" `shouldBe` expectedTokens
        
        it "Returns a multiply and a divide token" $ do
            let expectedTokens = [TokenDivide, TokenMultiply]
            scanTokens "/*" `shouldBe` expectedTokens
        
        it "Returns a Less and LessEqual token" $ do
            let expectedTokens = [TokenLess, TokenLessEqual]
            scanTokens "<<=" `shouldBe` expectedTokens

        it "Returns a Greater and GreaterEqual token" $ do
            let expectedTokens = [TokenGreater, TokenGreaterEqual]
            scanTokens ">>=" `shouldBe` expectedTokens
        
        it "Returns a print token" $ do
            let expectedTokens = [TokenPrint]
            scanTokens "print" `shouldBe` expectedTokens

        it "Returns an if, else and endif token" $ do
            let expectedTokens = [TokenIf, TokenElse, TokenEndIf]
            scanTokens "if else endif" `shouldBe` expectedTokens  

        it "Returns a while and an endwhile token" $ do 
            let expectedTokens = [TokenWhile, TokenEndWhile]
            scanTokens "while endwhile" `shouldBe` expectedTokens
