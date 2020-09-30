{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module MuseScore.Types
  ( ProgramInfo (..),
    Instrument (..),
    parseInst,
    Key (..),
    key,
    Score (..),
    parts,
    Part (..),
    Arrangement (..),
    DerivedScore (..),
  )
where

import           Data.Text as TE
import           Text.XML

-- This is our core data structure for representing a parsed score
data Score = Score
  { scname  :: TE.Text,
    scparts :: Arrangement,
    scinfo  :: ProgramInfo,
    scdoc   :: Document
  }
  deriving (Show)

-- Derived scores are *generated* scores that contain a single part, and thus a single instrument
data DerivedScore = DerivedScore
  { dsname :: TE.Text,
    dsinst :: Instrument,
    dsdoc  :: Document
  } deriving (Show)

-- An arrangement, as a set of parts
data Arrangement =
  ConcertHead { piano :: Part } |
  Classic { piano :: Part, sax :: Part, trumpet :: Part}
  deriving (Show)

parts :: Arrangement -> [Part]
parts (ConcertHead piano)         = [piano]
parts (Classic piano sax trumpet) = [piano, sax, trumpet]

-- A part extracted from a score.
data Part = Part
  { instrument :: Instrument,
    partnode   :: Node
  }

instance Show Part where
  show p = show (instrument p)

-- The various kinds of instrument we might find
data Instrument
  = Piano
  | AltoSax
  | Trumpet
  | Trombone
  | Clarinet
  | Drums
  | Guitar
  | Bass
  | Unknown TE.Text
  deriving (Eq, Ord)

instance Show Instrument where
  show Piano       = "Piano"
  show AltoSax     = "Alto Sax"
  show Trumpet     = "B♭ Trumpet"
  show (Unknown _) = "Unknown Instrument"

parseInst :: TE.Text -> Instrument
parseInst "Piano"          = Piano
parseInst "Alto Sax"       = AltoSax
parseInst "Alto Saxophone" = AltoSax
parseInst "B♭ Trumpet"     = Trumpet
parseInst t                = Unknown t

-- Their Keys
data Key = C | Eb | Bb deriving (Eq)

instance Show Key where
  show C  = "C"
  show Eb = "Eb"
  show Bb = "Bb"

key :: Instrument -> Key
key AltoSax  = Eb
key Trumpet  = Bb
key Piano    = C
key Clarinet = Bb
key _        = C

-- ProgramInfo, metadata from a musescore file
data ProgramInfo = ProgramInfo
  { programVersion      :: TE.Text,
    programVersionNode  :: Node,
    programRevision     :: TE.Text,
    programRevisionNode :: Node
  }

instance Show ProgramInfo where
  show p = "{ProgramVersion: " ++ (show . programVersion $ p) ++ ", ProgramRevision: " ++ (show . programRevision $ p) ++ "}"
