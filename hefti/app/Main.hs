module Main where


import           Data.Map            as DM
import           Data.Maybe
import           Data.Text           as TE
import           Environment
import           MuseScore.Compiler
import           MuseScore.Linker
import           MuseScore.Types
import           Options.Applicative
import           Path
import           Path.IO
import           Text.XML

getTestFile :: IO Document
getTestFile = Text.XML.readFile def "../src/A_Smooth_One.mscx"


main :: IO ()
main = do
  -- Get the working directory
  current_directory <- getCurrentDir

  -- Set up the environment
  env <- execParser (opts current_directory) >>= prepareEnvironment

  -- List the directory, and extract musescore files.
  score_paths <-
    listDir (source_d env)
      >>= (pure . snd)

  putStrLn "Compiling books from files: "
  mapM_ (\p -> putStrLn $ "\t - " ++ (show $ filename p)) score_paths

  -- read the files into document structures
  pathdocs <- (mapM (readMSFile env)) score_paths

  -- Convert the musescore files into internal score files
  -- We're just ignoring the files that fail for now. They shouldn't fail though...
  let scores = catMaybes $ Prelude.map score pathdocs

  -- Given the scores, and documents, convert them into a player arrangement
  let playarrs = Prelude.map (\(sc, (_, d)) -> substitute d sc) $ Prelude.zip scores pathdocs

  -- Generate MuseScore XML files for each part, and write them to disk
  msparts <- mapM (\sc -> partfiles env sc) playarrs

  -- Generate PDF files from the individual MuseScore XML parts
  pdfparts <- mapM (\sc -> pdffiles env sc) msparts

  let books = reshape pdfparts

  pdffiles <- mapM (catpdfs env) books

  mapM_ (\f -> putStrLn $ "Book written to : " ++ (show $ dat f)) pdffiles

  -- putStrLn $ show books
