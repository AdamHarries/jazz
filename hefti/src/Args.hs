{-# LANGUAGE QuasiQuotes #-}

module Args
  ( CmdLineArgs (..),
    opts,
  )
where

import Data.Semigroup ((<>))
import Options.Applicative
import Path
import Path.IO
import PathUtils

data CmdLineArgs = CmdLineArgs
  { source_d :: AbsDir,
    book_d :: AbsDir,
    build_d :: AbsDir,
    release_name :: String,
    mscore_path :: RelFile
  }
  deriving (Show)

parseDirectory :: AbsDir -> ReadM AbsDir
parseDirectory base = eitherReader $ eitherAppendPath base

parseFile :: ReadM RelFile
parseFile =
  eitherReader $ \path ->
    case parseRelFile path of
      Just p -> Right p
      Nothing -> Left ("Failed to parse relative path: " ++ path)

parseCmdLineArgs :: AbsDir -> Parser CmdLineArgs
parseCmdLineArgs base =
  let absDOption = parseDirectory base
   in CmdLineArgs
        <$> option
          absDOption
          ( long "source"
              <> short 's'
              <> value (appendPath base "src")
              <> metavar "SRC"
              <> help
                "Source directory for musescore (mscx, mscz) files (default - `$(pwd)/src`)"
          )
        <*> option
          absDOption
          ( long "book"
              <> short 'o'
              <> value (appendPath base "books")
              <> metavar "BOOK"
              <> help
                "Output folder to write (finished) books (default - `$(pwd)/books`)"
          )
        <*> option
          absDOption
          ( long "build"
              <> short 'b'
              <> value (appendPath base "build")
              <> metavar "BUILD"
              <> help
                "Temporary folder for storing intermediate files (default - `$(pwd)/build`)"
          )
        <*> strOption
          ( long "release"
              <> short 'r'
              <> value "unnamed"
              <> metavar "RELEASE"
              <> help "Version name for the generated books (default - `unnamed`)"
          )
        <*> option
          parseFile
          ( long "musescore"
              <> short 'm'
              <> value [relfile|mscore-portable|]
              <> metavar "MUSESCORE"
              <> help
                "Path to musescore command line program (default -`mscore-portable`)"
          )

opts :: AbsDir -> ParserInfo CmdLineArgs
opts base =
  info
    ((parseCmdLineArgs base) <**> helper)
    ( fullDesc
        <> progDesc "Generate pdf \"books\" of charts from individua musescore files in src"
        <> header "Hefti - an arranging tool for combining individual songs into full charts"
    )
