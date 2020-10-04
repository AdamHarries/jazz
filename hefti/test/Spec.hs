{-# LANGUAGE OverloadedStrings #-}

import           Data.Maybe
import qualified Data.Text        as DT
import           MuseScore.IO
import           MuseScore.Parser
import           MuseScore.Types
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
        let sc = fromJust (score testFile)
        describe "score name" $ do
            it "finds name in an example score" $ do
                (scname sc) `shouldBe` "A Smooth One"
        -- describe "parts" $ do
        --     it "score returns three parts" $ do
        --         (length $ scparts sc) `shouldBe` 3
        --     it "parts contains saxophone" $ do
        --         (containsPart AltoSax (scparts sc)) `shouldBe` True
        --     it "parts contains trumpet" $ do
        --         (containsPart Trumpet (scparts sc)) `shouldBe` True
        --     it "parts contains piano" $ do
        --         (containsPart Piano (scparts sc)) `shouldBe` True


