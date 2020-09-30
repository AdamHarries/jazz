{-# LANGUAGE OverloadedStrings #-}

module MuseScore.Partgen
  (
    -- genXmlParts,
  --   buildOutputPaths,
  --   scoreFilename,
  )
where

import           BuildEnvironment
import           Control.Lens
import           Data.Text        as TE
import           MuseScore.Types
import           PathUtils
import           Text.XML
import           Text.Xml.Lens


--- we need N different paths to handle each kind of arrangement.

generate :: IO ()
generate = do
  putStrLn "Generating...stuff."

filename :: DerivedScore -> TE.Text
filename score = let name = (dsname score)
                     suffix =  TE.pack $ show  $ key $ dsinst score
                     ext = ".mscx" in
  tidyFilename $ name `append` "_" `append` suffix `append` ext

generateParts :: Score -> [DerivedScore]
generateParts scr = let name = scname scr
                        doc = scdoc scr
                        prts = MuseScore.Types.parts $ scparts scr in
  Prelude.map (substitutePart name doc) prts where

  substitutePart :: TE.Text -> Document -> Part -> DerivedScore
  substitutePart name doc part  = DerivedScore {
    dsname = name,
    dsinst = instrument part,
    dsdoc = subScoreNode (doc) (partnode part)
  }
  subScoreNode :: Document -> Node -> Document
  subScoreNode doc (NodeElement e) = doc & xml.node "Score" .~ e
  subScoreNode doc _               = doc


buildOutputPaths :: BuildEnv -> TE.Text -> Instrument -> Maybe MuseScoreFilePath
buildOutputPaths env title instrument =
  (maybeAppendFile (parts_d env) filename) >>= asMuseScoreFilePath
  where
    filename :: String
    filename = (unpack title) ++ "_" ++ (show $ key instrument) ++ ".mscx"
