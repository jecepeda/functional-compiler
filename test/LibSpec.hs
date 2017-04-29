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