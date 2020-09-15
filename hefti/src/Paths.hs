{-# LANGUAGE OverloadedStrings #-}

module Paths
    (
        AbsDir,
        AbsFile,
        RelFile,
        maybeAppendPath,
        eitherAppendPath,
        appendPath,
        maybeAppendFile,
        eitherAppendFile,
        MuseScoreFilePath(..),
        asMuseScoreFilePath,
        tidyFilename
    ) where

import           Data.List.Split
import           Data.Text        as TE
import           Path
import           System.Directory
import           System.FilePath


type AbsDir = Path Abs Dir
type AbsFile = Path Abs File
type RelFile = Path Rel File

data MuseScoreFilePath
  = MSCX AbsFile
  | MSCZ AbsFile
  deriving (Show, Eq)


-- Various utilities for appending directory paths to absolute directories unsafely,
-- i.e. without using `resolveDir` or `resolveFile` to check (using IO) that they
-- actually exist
maybeAppendPath :: AbsDir -> FilePath -> Maybe AbsDir
maybeAppendPath base app =
   parseAbsDir $ normalise $  normaliseIndirection $ (toFilePath base) System.FilePath.</> app

eitherAppendPath :: AbsDir -> FilePath -> Either String AbsDir
eitherAppendPath base app = case maybeAppendPath base app of
    Just p  -> Right p
    Nothing -> Left ("Failed to parse path: " ++ app)

appendPath :: AbsDir -> FilePath -> AbsDir
appendPath base app = case maybeAppendPath base app of
    Just p  -> p
    Nothing -> base

-- Various utilities for appending file paths to absolute directories
maybeAppendFile :: AbsDir -> FilePath -> Maybe AbsFile
maybeAppendFile base app =
   parseAbsFile $ normalise $ normaliseIndirection $ (toFilePath base) System.FilePath.</> app

eitherAppendFile :: AbsDir -> FilePath -> Either String AbsFile
eitherAppendFile base app = case maybeAppendFile base app of
    Just p  -> Right p
    Nothing -> Left ("Failed to parse path: " ++ app)


-- Normalise path indirection, i.e.:
-- normaliseIndirection "/home/bar/../bar/foo/../foo" = "/home/bar/foo"
normaliseIndirection :: FilePath -> FilePath
normaliseIndirection fp =
  let elements = splitPath fp in
    normAdj (Prelude.head elements) (Prelude.tail elements) where
      normAdj :: FilePath -> [FilePath] -> FilePath
      normAdj fp [] = fp
      normAdj fp (x:[]) = fp System.FilePath.</> x
      normAdj fp (up:lower:xs) = if lower == "../" then
        normAdj fp xs
        else
          normAdj (fp System.FilePath.</> up) (lower : xs)

-- Transforms an absolute file path into an equivalent musescore filepath
asMuseScoreFilePath :: AbsFile -> Maybe MuseScoreFilePath
asMuseScoreFilePath f =
  let ex = Prelude.last $ Data.List.Split.splitOn "." $ toFilePath f
   in case ex of
        "mscx" -> Just $ MSCX f
        "mscz" -> Just $ MSCZ f
        _      -> Nothing

tidyFilename :: TE.Text -> TE.Text
tidyFilename = TE.concatMap replace where
  replace :: Char -> Text
  replace ' '  = "_"
  replace '('  = ""
  replace ')'  = ""
  replace ','  = ""
  replace '\'' = ""
  replace '\"' = ""
  replace c    = singleton c
