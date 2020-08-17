
module Main where

import           Args
import           BuildEnvironment    as BE
import           Data.Maybe
import           MuseScoreFile
import           Options.Applicative
import           Path
import           Path.IO
import           Paths

main :: IO ()
main = do
  current_directory <- getCurrentDir

  -- Set up the environment
  env <- execParser (opts current_directory) >>= prepareEnvironment
  -- List the directory, and extract musescore files.
  mscore_files <-
    listDir (BE.source_d env) >>= (pure . snd) >>=
    (pure . catMaybes . map asMuseScoreFilePath) >>=
    (mapM (generateXMLf env))
  putStrLn $ show mscore_files
  -- Generate PDF files
  -- Generate PDF/title pairs
  -- Generate LaTeX
  -- Compile
  putStrLn $ show env
