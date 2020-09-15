{-# LANGUAGE OverloadedStrings #-}
module MSPartGen (
    genXmlParts,
    buildOutputPaths,
    scoreFilename
)where

import           BuildEnvironment
import           Data.Text        as TE
import           MSTypes
import           Paths
import           Text.XML

data GenPart = GenPart {
    path     :: MuseScoreFilePath,
    document :: Document
}

-- data Score = Score {
--   scoreName  :: TE.Text,
--   scoreParts :: [Part],
--   scoreInfo  :: ProgramInfo
-- } deriving (Show)

-- genIndividualPart

genXmlParts :: BuildEnv -> Score -> [Score]
genXmlParts env score = Prelude.map extractPart (scoreParts score) where
    extractPart :: Part -> Score
    extractPart p = Score {
        scoreName = scoreName score,
        scoreParts = [p],
        scoreInfo = scoreInfo score
    }

scoreFilename :: Score -> TE.Text
scoreFilename score = tidyFilename ((scoreName score) `append` "_" `append` suffix `append` ".mscx") where
    suffix = case scoreParts score of
        (p:([])) -> textKey $ instKey $ instrument p
        _        -> ""

buildOutputPaths :: BuildEnv -> TE.Text -> Instrument -> Maybe MuseScoreFilePath
buildOutputPaths env title instrument =
    (maybeAppendFile (parts_d env) filename) >>= asMuseScoreFilePath
    where
        key = instKey instrument
        filename = (unpack title) ++ "_" ++ (show key) ++ ".mscx"
