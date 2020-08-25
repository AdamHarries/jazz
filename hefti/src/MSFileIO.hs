{-# LANGUAGE OverloadedStrings #-}

module MSFileIO(
    readMSFile
)where

import           BuildEnvironment  as BE
import           Control.Exception

import qualified Data.Text         as TE
import           MSTypes
import           Path
import           Paths
import           System.Process
import           Text.XML
import           Text.XML.Cursor
import           XMLUtils



-- Read a musescore file into an XML document
readMSFile :: BuildEnv -> MuseScoreFilePath -> IO Document
readMSFile _ (MSCX f) = Text.XML.readFile def (toFilePath f)
readMSFile be zf = do
  xmlmf <- convertToMSCX be zf
  case xmlmf of
    (MSCX f) -> Text.XML.readFile def (toFilePath f)
    _        -> pure (throw CouldNotGetMSCX)
  where
    -- Generates an (equivalent) temporary XML file for `.mscz` musescore files,
    -- does nothing for "normal" `.mscx` files
    convertToMSCX :: BuildEnv -> MuseScoreFilePath -> IO MuseScoreFilePath
    convertToMSCX _ xmlf@(MSCX _) = pure xmlf
    convertToMSCX args (MSCZ f) = do
      outputf <- Path.replaceExtension ".mscx" $ (BE.parts_d args) </> (filename f)
      shouldCreate <- shouldCreate f outputf
      if shouldCreate then
        callProcess (toFilePath $ BE.mscore_path args) ["-o", (toFilePath outputf), "-P", (toFilePath f)]
      else
        Prelude.putStrLn $  "File " ++ (show outputf) ++ " already exists"
      pure (MSCX outputf)
