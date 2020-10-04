
-- {-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes       #-}
module MuseScore.Types (
    Instrument (..),
    parseInst,
    Key (..),
    key,
    Score (..),

) where

import           Data.Text   as TE
import           Environment

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

-- This is our key data structure for representing a score, or parts in a score, or filenames to that
-- score, etc, etc. It behaves a bit like a generic AST, representing various parts of the compilation process.
data Score a = Score
  { name  :: TE.Text, -- the name of the score that this belongs to
    spath :: AbsFile, -- the original source file that this was derived from
    parts :: a -- the parts in this score
  }
  deriving (Show)

instance Functor Score where
  -- fmap :: (a -> b) -> (Score a) -> (Score b)
  fmap f sc = Score {
    name = name sc,
    spath = spath sc,
    parts = f $ parts sc
  }
  -- (<$) :: a -> f b -> f a
  (<$) p sc = Score {
    name = name sc,
    spath = spath sc,
    parts = p
  }
