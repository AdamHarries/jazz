module MuseScoreFile
(
    MuseScoreFilePath(..),
    asMuseScoreFilePath,
    generateXMLf

)where

import           BuildEnvironment  as BE
import           Control.Exception
import           Data.ByteString   as BS
import           Data.List.Split
import           Path
import           Paths
import           System.Process


data MuseScoreFilePath
  = XMLFile AbsFile
  | ZIPFile AbsFile
  deriving (Show, Eq)

asMuseScoreFilePath :: AbsFile -> Maybe MuseScoreFilePath
asMuseScoreFilePath f =
  let ex = Prelude.last $ splitOn "." $ toFilePath f
   in case ex of
        "mscx" -> Just $ XMLFile f
        "mscz" -> Just $ ZIPFile f
        _      -> Nothing

-- generatePdfs :: BuildEnv -> AbsFile -> IO ()
-- Generates an (equivalent) temporary XML file for `.mscz` musescore files,
-- do nothing for "normal" `.mscx` files
generateXMLf :: BuildEnv -> MuseScoreFilePath -> IO MuseScoreFilePath
generateXMLf _ xmlf@(XMLFile _) = pure xmlf
generateXMLf args (ZIPFile f) = do
  outputf <- Path.replaceExtension ".mscx" $ (BE.parts_d args) </> (filename f)
  shouldCreate <- shouldCreate f outputf
  if shouldCreate then
    callProcess (toFilePath $ BE.mscore_path args) ["-o", (toFilePath outputf), "-P", (toFilePath f)]
  else
    Prelude.putStrLn $  "File " ++ (show outputf) ++ " already exists"
  pure (XMLFile outputf)


readMSFile :: BuildEnv ->  MuseScoreFilePath -> IO ByteString
readMSFile _ (XMLFile f) = BS.readFile $ toFilePath f
readMSFile be zf = do
  xmlmf <- generateXMLf be zf
  case xmlmf of
    (XMLFile f) -> BS.readFile $ toFilePath f
    _           -> (throw CouldNotGetXMLFile) >> (pure empty)

