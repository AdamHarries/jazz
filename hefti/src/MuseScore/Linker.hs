module MuseScore.Linker () where

import           MuseScore.Types


data Book a = Book {
    instrument :: Instrument
}
