{-# LANGUAGE OverloadedStrings #-}
module MSTypes (
    ProgramInfo(..),
    Instrument(..),
    parseInstrument,
    Key(..),
    instKey,
    Score(..),
    Part(..),
)where

import           Data.Text as TE
import           Text.XML

-- This is our core data structure for representing a parsed score
data Score = Score {
  scoreName  :: TE.Text,
  scoreParts :: [Part],
  scoreInfo  :: ProgramInfo
} deriving (Show)

-- A part extracted from a score.
data Part = Part {
  instrument :: Instrument,
  xmlNode    :: Node
}

instance Show Part where
  show p = show (instrument p)

-- The various kinds of instrument we might find
data Instrument = Piano |
  AltoSax |
  Trumpet |
  Trombone |
  Clarinet |
  Drums |
  Guitar |
  Bass |
  Unknown TE.Text deriving (Eq, Show)

parseInstrument :: TE.Text -> Instrument
parseInstrument "Piano"          = Piano
parseInstrument "Alto Sax"       = AltoSax
parseInstrument "Alto Saxophone" = AltoSax
parseInstrument "B♭ Trumpet"     = Trumpet
parseInstrument t                = Unknown t

-- Their Keys
data Key = C | Eb | Bb deriving (Eq, Show)
instKey :: Instrument -> Key
instKey AltoSax  = Eb
instKey Trumpet  = Bb
instKey Piano    = C
instKey Clarinet = Bb
instKey _        = C


-- ProgramInfo, metadata from a musescore file
data ProgramInfo = ProgramInfo {
  programVersion      :: TE.Text,
  programVersionNode  :: Node,
  programRevision     :: TE.Text,
  programRevisionNode :: Node
}

instance Show ProgramInfo where
  show p = "ProgramVersion: " ++ (show . programVersion $ p) ++ ", ProgramRevision: " ++ (show . programRevision $ p)
