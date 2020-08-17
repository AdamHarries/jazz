{-# LANGUAGE QuasiQuotes #-}

module Args
(
    CmdLineArgs(..),
    opts
)where

import           Data.Semigroup      ((<>))
import           Options.Applicative
import           Path
import           Path.IO
import           Paths

data CmdLineArgs =
  CmdLineArgs
    { source_d     :: AbsDir
    , book_d       :: AbsDir
    , build_d      :: AbsDir
    , release_name :: String
    , mscore_path  :: RelFile
    }
  deriving (Show)

parseDirectory :: AbsDir -> ReadM AbsDir
parseDirectory base = eitherReader $ eitherAppendPath base

parseFile :: ReadM RelFile
parseFile = eitherReader $ \path ->
  case parseRelFile path of
    Just p  -> Right p
    Nothing -> Left ("Failed to parse relative path: " ++ path)

parseCmdLineArgs :: AbsDir -> Parser CmdLineArgs
parseCmdLineArgs base =
  let absDOption = parseDirectory base
   in CmdLineArgs <$>
      option
        absDOption
        (long "source" <>
         short 's' <>
         value (appendPath base "src") <>
         metavar "SRC" <>
         help "Source directory for musescore (mscx, mscz) files.") <*>
      option
        absDOption
        (long "book" <>
         short 'o' <>
         value (appendPath base "books") <>
         metavar "BOOK" <>
         help "Source directory for musescore (mscx, mscz) files.") <*>
      option
        absDOption
        (long "build" <>
         short 'b' <>
         value (appendPath base "build") <>
         metavar "BUILD" <>
         help "Source directory for musescore (mscx, mscz) files.") <*>
      strOption
        (long "release" <>
         short 'r' <>
         value "unnamed" <>
         metavar "RELEASE" <>
         help "Source directory for musescore (mscx, mscz) files.") <*>
      option parseFile
        (long "musescore" <>
         short 'm' <>
         value [relfile|mscore-portable|] <>
         metavar "MUSESCORE" <>
         help "Source directory for musescore (mscx, mscz) files.")

opts :: AbsDir -> ParserInfo CmdLineArgs
opts base =
  info
    ((parseCmdLineArgs base) <**> helper)
    (fullDesc <>
     progDesc "Print a greeting for TARGET" <>
     header "hello - a test for optparse-applicative")
