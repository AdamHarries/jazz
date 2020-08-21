{-# LANGUAGE OverloadedStrings #-}
module MuseScoreFile
(
    MuseScoreFilePath(..),
    asMuseScoreFilePath,
    readMSFile,
    generateXMLf,
    getTestFile,
    findNodes,
    scores,
    programInfo,
    findScoreName

)where

import           BuildEnvironment                  as BE
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Writer.Strict
import           Data.List.Split
import           Data.Text                         as TE
import           Path
import           Paths
import           System.Process
import           Text.XML
import           Text.XML.Cursor



data MuseScoreFilePath
  = XMLFile AbsFile
  | ZIPFile AbsFile
  deriving (Show, Eq)

asMuseScoreFilePath :: AbsFile -> Maybe MuseScoreFilePath
asMuseScoreFilePath f =
  let ex = Prelude.last $ Data.List.Split.splitOn "." $ toFilePath f
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


readMSFile :: BuildEnv ->  MuseScoreFilePath -> IO Document
readMSFile _ (XMLFile f) = Text.XML.readFile def (toFilePath f)
readMSFile be zf = do
  xmlmf <- generateXMLf be zf
  case xmlmf of
    (XMLFile f) -> Text.XML.readFile def (toFilePath f)
    _           -> pure (throw CouldNotGetXMLFile)

getTestFile :: IO Document
getTestFile = Text.XML.readFile def "../src/A_Smooth_One.mscx"

makeName :: Text -> Name
makeName t = Name { nameLocalName = t, nameNamespace = Nothing, namePrefix = Nothing}

-- findScoreName :: Document -> [Node]
-- findScoreName d = findNodesBy d isTextWithScoreName where
--   textNodeName = makeName "Text"
--   isTextWithScoreName :: Node -> Bool
--   isTextWithScoreName n@(NodeElement e) = True
--   isTextWithScoreName _ = False


findNodesBy :: Document -> (Node -> Bool) -> [Node]
findNodesBy d check = execWriter $ findNodesHelper (fromDocument d) where
    findNodesHelper :: Cursor -> Writer [Node] ()
    findNodesHelper c = do
      when (check $ node c) $ tell [node c]
      mapM_ findNodesHelper (child c)


findNodes :: Document -> Text -> [Node]
findNodes d n = findNodesBy d checkName where
    fullName = makeName n
    checkName :: Node -> Bool
    checkName (NodeElement e) = (elementName e) == fullName
    checkName _               = False


findScoreName :: Document -> Text
findScoreName d = getTitle $ Prelude.concatMap (unwrapN . node) $
  descendant (fromDocument d) >>=
    check (\cur -> case node cur of
      (NodeContent t) -> (t == "Title")
      _               -> False) >>=
    Text.XML.Cursor.parent >>=
    Text.XML.Cursor.parent >>=
    child >>=
    element "text" >>= 
    descendant where
  unwrapN (NodeContent c) = [c]
  unwrapN _ = [] 
  getTitle (t:_) = strip t 
  getTitle [] = "Unknown title"



scores :: Document -> [Node]
scores d = searchDocument d "Score"

-- Get program information (i.e. MuseScore program info) if it exists
programInfo :: Document -> [Node]
programInfo d = (searchDocument d "programVersion") ++(searchDocument d "programRevision") where

-- Search a document for a specifically named node.
searchDocument :: Document -> Name -> [Node]
searchDocument d name = Prelude.map node $ descendant (fromDocument d) >>= element name
