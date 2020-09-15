
module Main where

import           Args
import           BuildEnvironment    as BE
import           Data.Maybe
import           MSFileIO
import           MSFileParser
import           MSPartGen
import           MSTypes
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
  score_paths <-
    listDir (BE.source_d env) >>=
    (pure . snd) >>=
    (pure . catMaybes . map asMuseScoreFilePath)

  documents <- (mapM (readMSFile env)) score_paths

  -- Convert the musescore files into internal score files
  let scores = map score documents

  -- Then convert them to a set of parts
  let parts = scores >>= (genXmlParts env)

  mapM_ (\sc -> do
    putStrLn $ "Sc: " ++ (show sc)
    putStrLn $ "Path: " ++ (show $ scoreFilename ( sc))
    ) parts

  -- Generate PDF files
  -- Generate PDF/title pairs
  -- Generate LaTeX
  -- Compile
  putStrLn $ show env

