{-# LANGUAGE OverloadedStrings #-}

module MuseScore.Parser
  ( score,
  )
where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Writer.Strict
import           Data.List.Split
import qualified Data.Map                          as DM
import           Data.Maybe
import qualified Data.Text                         as TE
import           MuseScore.Types
import           Path
import           PathUtils
import           System.Process
import           Text.XML
import           Text.XML.Cursor
import           XMLUtils
-- Parse an XML document into a score
score :: Document -> Maybe Score
score doc = do
  arr <- arrangement doc
  let name = findScoreName doc
  let info = programInfo doc
  Just $ Score
    { scname = findScoreName doc,
      scparts = arr,
      scinfo = programInfo doc,
      scdoc = doc
    }

-- Given a document, parse the name of the score
findScoreName :: Document -> TE.Text
findScoreName d =
  getTitle $
    Prelude.concatMap (unwrapN . node) $
      descendant (fromDocument d)
        >>= check
          ( \cur -> case node cur of
              (NodeContent t) -> (t == "Title")
              _               -> False
          )
        >>= Text.XML.Cursor.parent
        >>= Text.XML.Cursor.parent
        >>= child
        >>= element "text"
        >>= descendant
  where
    unwrapN (NodeContent c) = [c]
    unwrapN _               = []
    getTitle (t : _) = TE.strip t
    getTitle []      = "Unknown title"

-- Parse an XML document to an "arrangement"
arrangement :: Document -> Maybe Arrangement
arrangement doc = listToMaybe $ catMaybes [classicArr scorenodes, concertHead scorenodes]
  where
    scorenodes :: DM.Map Instrument Node
    scorenodes = DM.fromList $ Prelude.concatMap namedScore $ searchDoc doc "Score"

    namedScore :: Node -> [(Instrument, Node)]
    namedScore n = case instrumentNames n of
      (te:[]) -> [(te, n)]
      _       -> []

    getpart :: Instrument -> DM.Map Instrument Node -> Maybe Part
    getpart i m  = do
      node <- DM.lookup i m
      Just (Part { instrument = i, partnode = node})

    concertHead :: DM.Map Instrument Node -> Maybe Arrangement
    concertHead m = do
      piano <- getpart Piano m
      Just (ConcertHead {piano = piano})

    classicArr :: DM.Map Instrument Node -> Maybe Arrangement
    classicArr m = do
      piano <- getpart Piano m
      sax <- getpart AltoSax m
      trumpet <- getpart Trumpet m
      Just (Classic {
        piano = piano,
        sax = sax,
            trumpet = trumpet
      })



-- parts doc = (subscores doc) >>= createPart
--   where
--     createPart :: Node -> [Part]
--     createPart n = case instrumentNames n of
--       (inst : []) -> [Part {instrument = inst, xmlNode = n}]
--       _           -> []

-- Given a node, parse the instruments beneath the node
-- Return a list, as we might have multiple sub-instruments
-- (e.g. in a score node with multiple sub scores)
instrumentNames :: Node -> [Instrument]
instrumentNames n =
  Prelude.map parseInst $
  Prelude.concatMap ( nodeContent . node) $
    descendant (fromNode n)
      >>= element "Instrument"
      >>= descendant
      >>= element "longName"

-- Get program information (i.e. MuseScore program info) if it exists
programInfo :: Document -> ProgramInfo
programInfo doc =
  ProgramInfo
    { programVersion = head $ nodeContent versionNode,
      programVersionNode = versionNode,
      programRevision = head $ nodeContent revisionNode,
      programRevisionNode = revisionNode
    }
  where
    versionNode = searchDocOrElse doc "programVersion" (defaultNodeWithContent "programVersion" "3.1.0")
    revisionNode = searchDocOrElse doc "programRevision" (defaultNodeWithContent "programRevision" "fb3c202")
