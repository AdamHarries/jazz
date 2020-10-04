{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module MuseScore.Compiler
  (
    readMSFile,
    score,
    substitute,
    partfiles,
    pdffiles,
    PDFArrangement(..)
  )
where

import           Control.Exception
import           Control.Lens                      ((&), (.~))
import           Control.Monad
import           Control.Monad.Trans.Writer.Strict
import           Data.List.Split
import qualified Data.Map                          as DM
import           Data.Maybe
import           Data.Text                         as TE
import           Debug.Trace
import           Environment
import           MuseScore.Types
import           Path
import           System.Process
import           Text.XML
import           Text.XML.Cursor
import qualified Text.Xml.Lens                     as XLens

-- Overview: The way this application works is split into two distinct stages, a compilation stage,
-- and a linking stage. For those of you familiar with C/C++ compilers, or even just compiling C/C+
-- programs, this will seem familar. Essentially, our "compilation" stage takes a number of "source
-- files", generates (from them) a number of pre-processed intermediate files, and finally generates
-- (from them) a set of "object files". In this application, they are (respectively) MuseScore score
-- files (containing multiple instruments each), MuseScore part files (containing a single
-- instrument per file), and PDF files (representing the objects at the end of our compilation.)

-- Once these PDF "object" files have been generated, the "linking" stage takes them and "links"
-- them together into books, in the same way that a C++ linker would link object files into a shared
-- library or executable.

-- These stages are kept separate as there is an inherent "transposition" in between the two, when
-- we go from a pipeline that handles structures that are of shape (roughly)
-- [(ScoreName, [(Instrument, Object)])] into a pipeline that handles structures that are of shape
-- (roughly, again) [(Instrument, [(ScoreName, Object)])]. During the compiler we care about
-- splitting/generating *per score*, wheras afterwards we care about linking *per instrument*.

-- Step 1. We want to parse a single MuseScore XML document into a freshly-parsed score, which contains
-- the various parts as simple nodes in the XML tree.
-- We represent the type of this arrangement mapping as a "Conductors" arrangement - instruments
-- to nodes in the score document
type CArrangement = DM.Map Instrument Node
score :: (AbsFile, Document) -> Maybe (Score CArrangement)
score (mspath, doc) = do
  arr <- arrangement doc
  name <- findScoreName doc
  Just $ Score
    { name = Prelude.head $ TE.lines $ name,
      spath = mspath,
      parts = arr
    }

  where
  -- Given a document, try to parse the name of the score
  findScoreName :: Document -> Maybe TE.Text
  findScoreName d =
      listToMaybe $ Prelude.concatMap (content . node) $
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
      content (NodeContent c) = [c]
      content _               = []

  -- Given a document, try and parse the various sub-scores (which each should only represent a
  -- single instrument) out of the document
  arrangement :: Document -> Maybe CArrangement
  arrangement doc = case Prelude.concatMap parts $ searchDoc doc "Score" of
    [] -> Nothing
    cs -> Just $ DM.fromList cs
    where
      -- Given a "Score" node, find the instrument(s) beneath this node, and construct a
      -- part from it/them. We only care about single-instrument Scores, so discard any that
      -- are not lists of single parts.
      parts :: Node -> [(Instrument, Node)]
      parts n = case instrumentNames n of
        (te:[]) -> [(te, n)]
        _       -> []
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
      -- Extract the text from a content node
      nodeContent :: MonadPlus m => Node -> m TE.Text
      nodeContent (NodeElement e) = case (Prelude.head . elementNodes $ e) of
        (NodeContent c) -> pure c
        _               -> mzero
      nodeContent _ = mzero
      -- Search a document for any of a specifically named node.
      searchDoc :: Document -> Name -> [Node]
      searchDoc d name = Prelude.map node $ descendant (fromDocument d) >>= element name



-- Step 2. Substitute nodes into the document, to produce a document for each instrument. We call this
-- arrangement mapping the "Players" arrangement - instruments are mapped to individual documents.
type PArrangement = DM.Map Instrument Document
substitute :: Document -> Score CArrangement -> Score PArrangement
substitute doc scr =
  fmap (DM.map (\n -> subScoreNode doc n)) scr
   where
    subScoreNode :: Document -> Node -> Document
    subScoreNode doc (NodeElement e) = doc & XLens.xml. XLens.node "Score" .~ e
    subScoreNode doc _               = doc

-- Step 3. Given an arrangement of XML documents, figure out where we would like to save them and save them there.
-- Here, our mapping is in terms of a "MuseScore" arrangement - i.e. Instrument to MuseScore file path
type MSCXPath = AbsFile
type MSArrangement = DM.Map Instrument MSCXPath
partfiles :: BuildEnv -> Score PArrangement -> IO (Score MSArrangement)
partfiles env scr = do
    -- map over the documents, and pair them with a filename
    let sourcefile = spath scr
    let filesWDocs = DM.mapWithKey (getMSCXPath $ filename sourcefile ) $ parts scr
    -- Write the documents out to the given filenames
    mapM_ (\(inst, (f, doc)) -> writeMSFile sourcefile f doc) $ DM.toList filesWDocs
    -- Discard the documents, and return a new score with the generated filenames
    pure $ (DM.map (\(f, doc) -> f) filesWDocs) <$ scr
  where
    -- Turn a source file name into an intermediate/build file name with an attached instrument
    getMSCXPath :: RelFile -> Instrument -> Document -> (MSCXPath, Document)
    getMSCXPath srcfile inst doc = do
      let keyex = "." ++ (show $ key inst)
      let mscxext = ".mscx"
      -- let ext = trace ((show $ key inst) ++ ".mscx") $ (show $ key inst) ++ ".mscx"
      case replaceExtension keyex srcfile >>= addExtension mscxext of
        Just f -> ((parts_d env) </> f, doc)
        _      -> throw (IllegalMSCXFilePath $ pack $ show srcfile)
    writeMSFile :: AbsFile -> MSCXPath -> Document -> IO ()
    writeMSFile srcfile partfile doc =
      makeRule srcfile partfile (\_ t -> Text.XML.writeFile def (toFilePath t) doc)


-- Step 4. Given an arrangement of MuseScore file paths, take them and convert them to PDF files,
-- saving them at a computed file path, and returning those paths.
type PDFPath = AbsFile
type PDFArrangement = DM.Map Instrument PDFPath
pdffiles :: BuildEnv -> Score MSArrangement -> IO (Score PDFArrangement)
pdffiles env scr = do
    let pathPairs = Prelude.map (\(k, p) -> (k, p, getPdfPath env p)) $ DM.toList (parts scr)
    pdfpaths <- mapM (\(k, mp, pp) -> (convertToPDF env mp pp) >> (pure (k, pp))) pathPairs
    pure $ (DM.fromList pdfpaths) <$ scr
  where
    getPdfPath :: BuildEnv -> MSCXPath -> PDFPath
    getPdfPath env mscxfile = do
      let basename = filename mscxfile
      let pdfext = ".pdf"
      case replaceExtension pdfext basename of
        Just f -> (pdf_d env) </> f
        _      -> throw (IllegalMSCXFilePath $ pack $ show mscxfile)

    convertToPDF :: BuildEnv -> MSCXPath -> PDFPath -> IO ()
    convertToPDF env mscxpath pdfpath =
      makeRule mscxpath pdfpath (\m p ->
        callProcess (toFilePath $ mscore_path env) [(toFilePath m), "-o", (toFilePath p)])

-- An IO utility to read a musescore file into an XML document
readMSFile :: BuildEnv -> AbsFile -> IO (AbsFile, Document)
readMSFile be zf = do
  xmlmf <- convertToMSCX be zf
  doc <- Text.XML.readFile def (toFilePath xmlmf)
  pure ( xmlmf, doc)
  where
    -- Generates an (equivalent) temporary XML file for `.mscz` musescore files,
    -- does nothing for "normal" `.mscx` files
    convertToMSCX :: BuildEnv -> AbsFile -> IO AbsFile
    convertToMSCX env f = case fileExtension f of
      Just ".mscx" -> pure f
      Just ".mscz" -> do
        outputf <- Path.replaceExtension ".mscx" $ (build_d env) </> (filename f)
        makeRule f outputf (\s t ->
          callProcess (toFilePath $ mscore_path env) ["-o", (toFilePath t), "-P", (toFilePath s)])
        pure outputf
      _ -> throw CouldNotGetMSCX

