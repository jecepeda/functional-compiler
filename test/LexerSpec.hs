module LexerSpec 
where

import Test.Hspec
import Test.QuickCheck
import Tokens

lexerSpec :: IO()
lexerSpec = hspec $ do
    describe "Lexer" $ do
        it "Returns an integer token" $ do
            let expectedTokens = [(TokenInt 123)]
            (scanTokens "123") `shouldBe` expectedTokens
        
        it "Returns a var and a string" $ do
            let expectedTokens = [TokenVar, (TokenSym "helloWorld")]
            (scanTokens "var helloWorld") `shouldBe` expectedTokens
        
        it "Returns only an new line token given enter" $ do
            let expectedTokens = [TokenNewLine]
            (scanTokens "\n") `shouldBe` expectedTokens
        
        it "Returns only a new line token given semicolon" $ do
            let expectedTokens = [TokenNewLine]
            (scanTokens ";") `shouldBe` expectedTokens
        
        it "Returns a plus and a minus token" $ do
            let expectedTokens = [TokenPlus, TokenMinus]
            (scanTokens "+-") `shouldBe` expectedTokens
        
        it "Returns all tokens together" $ do
            let expectedTokens = [(TokenInt 123), 
                                  TokenVar, 
                                  (TokenSym "helloWorld"), 
                                  TokenNewLine, 
                                  TokenPlus, 
                                  TokenMinus]
            scanTokens "123 var helloWorld\n\n\n +-" `shouldBe` expectedTokens