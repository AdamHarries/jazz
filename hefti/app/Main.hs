module Main where


import           Data.Map            as DM
import           Data.Maybe
import           Data.Text           as TE
import           Environment
import           MuseScore.Compiler
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

  mapM_ (putStrLn . show) score_paths

  -- read the files into document structures
  pathdocs <- (mapM (readMSFile env)) score_paths

  -- Convert the musescore files into internal score files
  -- We're just ignoring the files that fail for now. They shouldn't fail though...
  let scores = catMaybes $ Prelude.map score pathdocs

  mapM_ (\sc -> putStrLn $ "Read title: " ++ (unpack $ name sc)) scores

  -- Given the scores, and documents, convert them into a player arrangement
  let playarrs = Prelude.map (\(sc, (_, d)) -> substitute d sc) $ Prelude.zip scores pathdocs

  -- Generate MuseScore XML files for each part, and write them to disk
  msparts <- mapM (\sc -> partfiles env sc) playarrs

  -- Generate PDF files from the individual MuseScore XML parts
  -- pdfparts <-
  mapM_ (\sc -> pdffiles env sc) msparts

  -- mapM_ (\sc -> do
  --   putStrLn $ "For Score: " ++ (unpack $ name sc)
  --   mapM_ (\(k, v) -> putStrLn $ "  " ++  (show $ key $ k) ++ " -> " ++ (show v)) $ DM.toList $  parts sc
  --   ) pdfparts

  -- Recursively zip the two together, so that we can write them out.

  -- -- Then convert them to a set of parts
  -- let parts = scores >>= (genXmlParts env)

  -- mapM_
  --   ( \sc -> do
  --       putStrLn $ "Sc: " ++ (show sc)
  --       putStrLn $ "Path: " ++ (show $ scoreFilename (sc))
  --   )
  --   parts

  -- Generate PDF files
  -- Generate PDF/title pairs
  -- Generate LaTeX
  -- Compile
  putStrLn $ show env
