{-# LANGUAGE OverloadedStrings #-}
module MSFileParser
(
    score,
)where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Writer.Strict
import           Data.List.Split
import qualified Data.Map                          as DM
import qualified Data.Text                         as TE
import           MSTypes
import           Path
import           Paths
import           System.Process
import           Text.XML
import           Text.XML.Cursor
import           XMLUtils

-- Parse a document into a score
score :: Document -> Score
score doc = Score {
  scoreName = findScoreName doc,
  scoreParts = parts doc,
  scoreInfo = programInfo doc
}

-- Given a document, parse the name of the score
findScoreName :: Document -> TE.Text
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
  unwrapN _               = []
  getTitle (t:_) = TE.strip t
  getTitle []    = "Unknown title"

-- Parse a document to a list of parts
parts :: Document -> [Part]
parts doc = (subscores doc) >>= createPart where
  createPart :: Node -> [Part]
  createPart n = case findInstruments n of
    (inst:[]) -> [Part {instrument = inst, xmlNode = n}]
    _         -> []

-- Given a node, parse the instruments beneath the node
findInstruments :: Node -> [Instrument]
findInstruments n = Prelude.concatMap (getNodeContent . node) $
  descendant (fromNode n) >>=
  element "Instrument" >>=
  descendant >>=
  element "longName"
  where
    getNodeContent (NodeElement e) = case (head . elementNodes $ e) of
      (NodeContent c) -> [parseInstrument c]
      _               -> []
    _ = []

-- find the scores within this node
subscores :: Document -> [Node]
subscores d = searchDoc d "Score"

-- Get program information (i.e. MuseScore program info) if it exists
programInfo :: Document -> ProgramInfo
programInfo doc = ProgramInfo {
    programVersion = getNodeContent versionNode,
    programVersionNode = versionNode,
    programRevision =  getNodeContent revisionNode,
    programRevisionNode = revisionNode
  }
  where
    versionNode = searchDocOrElse doc "programVersion" (defaultNodeWithContent "programVersion" "3.1.0")
    revisionNode = searchDocOrElse doc "programRevision" (defaultNodeWithContent "programRevision" "fb3c202")
    getNodeContent (NodeElement e) = case (head . elementNodes $ e) of
      (NodeContent c) -> c

