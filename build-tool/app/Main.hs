module Main where

import           Control.Exception
import           Control.Monad.Catch
import           Data.Semigroup      ((<>))
import           Options.Applicative
import           Path
import           System.Directory
import           Type.Reflection

import           Lib

data BuildException
  = NoSourceDir
  | NoSourceFiles
  deriving (Show, Typeable)

instance Exception BuildException

type TDirectory = Path Abs Dir

type TFile = Path Abs File

data BuildArgs =
  BuildArgs
    { source_d     :: TDirectory
    , book_d       :: TDirectory
    , build_d      :: TDirectory
    , release_name :: String
    , mscore_path  :: String
    }
  deriving (Show)

parseDirectory :: TDirectory -> ReadM TDirectory
parseDirectory base =
  eitherReader $ \path ->
    case parseRelDir path of
      Just fp -> Right (base </> fp)
      Nothing -> Left ("Failed to parse path " ++ path)

defaultDir :: TDirectory -> FilePath -> TDirectory
defaultDir base path =
  case parseRelDir path of
    Just fp -> base </> fp
    Nothing -> base

parseBuildArgs :: TDirectory -> Parser BuildArgs
parseBuildArgs base =
  let absDOption = parseDirectory base
   in BuildArgs <$>
      option
        absDOption
        (long "source" <>
         short 's' <>
         value (defaultDir base "src") <>
         metavar "SRC" <>
         help "Source directory for musescore (mscx, mscz) files.") <*>
      option
        absDOption
        (long "book" <>
         short 'o' <>
         value (defaultDir base "books") <>
         metavar "BOOK" <>
         help "Source directory for musescore (mscx, mscz) files.") <*>
      option
        absDOption
        (long "build" <>
         short 'b' <>
         value (defaultDir base "build") <>
         metavar "BUILD" <>
         help "Source directory for musescore (mscx, mscz) files.") <*>
      strOption
        (long "release" <>
         short 'r' <>
         value "unnamed" <>
         metavar "RELEASE" <>
         help "Source directory for musescore (mscx, mscz) files.") <*>
      strOption
        (long "musescore" <>
         short 'm' <>
         value "mscore" <>
         metavar "MUSESCORE" <>
         help "Source directory for musescore (mscx, mscz) files.")

opts :: TDirectory -> ParserInfo BuildArgs
opts base =
  info
    ((parseBuildArgs base) <**> helper)
    (fullDesc <>
     progDesc "Print a greeting for TARGET" <>
     header "hello - a test for optparse-applicative")

prepareEnvironment :: BuildArgs -> IO ()
prepareEnvironment options = do
  source_ex <- doesDirectoryExist $ toFilePath $ source_d options
  case source_ex of
    True  -> pure ()
    False -> throw NoSourceDir
  createDirectoryIfMissing True $ toFilePath $ build_d options -- create build
  createDirectoryIfMissing True $ toFilePath $ book_d options -- and book

main :: IO ()
main = do
  current_directory <- getCurrentDirectory >>= parseAbsDir
  options <- execParser (opts current_directory)
  -- Set up the environment
  prepareEnvironment options
  -- Get the files
  -- Generate PDF files
  -- Generate PDF/title pairs
  -- Generate LaTeX
  -- Compile
  putStrLn $ show options
