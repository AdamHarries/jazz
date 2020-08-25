{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as DT
import           MSFileIO
import           MSFileParser
import           MSTypes
import           Test.Hspec
import           Text.XML
import           XMLUtils

getTestFile :: IO Document
getTestFile = Text.XML.readFile def "../src/A_Smooth_One.mscx"

containsPart :: Instrument -> [Part] -> Bool
containsPart i []     = False
containsPart i (p:ps) = (i == instrument p) || (containsPart i ps)

main :: IO ()
main = do
    testFile <- getTestFile
    hspec $ do
        let sc = score testFile
        describe "score name" $ do
            it "finds name in an example score" $ do
                (scoreName sc) `shouldBe` "A Smooth One"
        describe "program info" $ do
            let info = scoreInfo sc
            let pv = programVersion info
            let pr = programRevision info
            it "programVersion is accurate" $ do
                (programVersionNode info) `shouldBe` (defaultNodeWithContent "programVersion" pv)
            it "programRevision is accurate" $ do
                (programRevisionNode info) `shouldBe` (defaultNodeWithContent "programRevision" pr)
        describe "parts" $ do
            it "score returns three parts" $ do
                (length $ scoreParts sc) `shouldBe` 3
            it "parts contains saxophone" $ do
                (containsPart AltoSax (scoreParts sc)) `shouldBe` True
            it "parts contains trumpet" $ do
                (containsPart Trumpet (scoreParts sc)) `shouldBe` True
            it "parts contains piano" $ do
                (containsPart Piano (scoreParts sc)) `shouldBe` True


