{-# LANGUAGE QuasiQuotes #-}

module BuildEnvironment
(
    BuildException(..),
    BuildEnv(..),
    prepareEnvironment,
    shouldCreate

)where

import           Args
import           Control.Exception
import           Path
import           Path.IO
import           Paths
import           Type.Reflection

data BuildException
  = NoSourceDir
  | NoSourceFiles
  | CannotFindMuseScore
  | CouldNotGetMSCX
  deriving (Show, Typeable)

instance Exception BuildException

data BuildEnv = BuildEnv {
  source_d     :: AbsDir,
  book_d       :: AbsDir,
  build_d      :: AbsDir,
  parts_d      :: AbsDir,
  tex_d        :: AbsDir,
  pdf_d        :: AbsDir,
  release_name :: String,
  mscore_path  :: AbsFile
} deriving (Show)


prepareEnvironment :: CmdLineArgs -> IO BuildEnv
prepareEnvironment options = do
  source_ex <- doesDirExist $ Args.source_d options
  if source_ex then
    putStrLn "Found source directory"
  else
    throw NoSourceDir
  absMScorePath <- (findExecutable $ Args.mscore_path options) >>= \apath ->
    case apath of
      Just p -> putStrLn ("Found musescore executable at " ++ (show p)) >> pure p
      Nothing -> throw CannotFindMuseScore
  ensureDir $ Args.build_d options -- create build
  ensureDir $ Args.book_d options -- and book
  let parts_d = (Args.build_d options) </> [reldir|parts|]
  let tex_d = (Args.build_d options) </> [reldir|tex|]
  let pdf_d = (Args.build_d options) </> [reldir|pdf|]
  ensureDir $ parts_d
  ensureDir $ tex_d
  ensureDir $ pdf_d
  pure (BuildEnv
   (Args.source_d options)
   (Args.book_d options)
   (Args.build_d options)
   parts_d
   tex_d
   pdf_d
   (Args.release_name options)
   absMScorePath)


-- Check if a `target` file should be created, based on a `source` file, a-la makefiles
shouldCreate :: AbsFile -> AbsFile -> IO Bool
shouldCreate source target = do
  targetExists <- doesFileExist target
  if targetExists then do
    sourceTime <- getAccessTime source
    targetTime <- getAccessTime target
    pure (targetTime < sourceTime)
  else
    pure True
