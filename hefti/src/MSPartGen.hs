module MSPartGen (

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

genXmlParts :: BuildEnv -> Score -> Score
genXmlParts = undefined

buildOutputPaths :: BuildEnv -> TE.Text -> Instrument -> Maybe MuseScoreFilePath
buildOutputPaths env title instrument =
    (maybeAppendFile (parts_d env) filename) >>= asMuseScoreFilePath
    where
        key = instKey instrument
        filename = (unpack title) ++ "_" ++ (show key) ++ ".mscx"
