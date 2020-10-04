{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module MuseScore.Linker (
    Book(..),
    reshape,
    catpdfs
) where

import           Control.Exception
import           Data.Map            as DM
import           Data.Map.Merge.Lazy as DMML
import           Data.Set            as DS
import           Data.Text           as TE
import qualified Data.Text.IO        as TEIO
import           Environment
import           MuseScore.Compiler
import           MuseScore.Types
import           System.Process

import           Path
import           Text.RawString.QQ

data Book a = Book {
    instrument :: Instrument,
    dat        :: a,
    songs      :: DM.Map TE.Text AbsFile
} deriving (Show)

instance Functor Book where
  -- fmap :: (a -> b) -> (Score a) -> (Score b)
  fmap f bk = Book {
    instrument = instrument bk,
    dat = f $ dat bk,
    songs = songs bk
  }
  -- (<$) :: a -> f b -> f a
  (<$) p bk = Book {
    instrument = instrument bk,
    dat = p,
    songs = songs bk
  }

-- Step one of linking: Reshape a list of shape  [(ScoreName, [(Instrument, AbsFile)])] into a list
-- of shape [(Instrument, [(ScoreName, AbsFile)])]
reshape :: [Score PDFArrangement] -> [Book ()]
reshape scrs = m2b $ mergeMaps $ Prelude.map transpose scrs
    where
        m2b :: DM.Map Instrument [(TE.Text, AbsFile)] -> [Book ()]
        m2b mp = Prelude.map (\(i, ps) -> Book {
            instrument = i,
            dat = (),
            songs = DM.fromList  ps
        })  $ DM.toList mp

        transpose :: Score PDFArrangement -> DM.Map Instrument [(TE.Text, AbsFile)]
        transpose scr = let n = (name scr) in
            DM.fromList $ Prelude.map (\(i, p) -> (i, [(n,p)])) $ DM.toList $ parts scr

        mergeMaps :: [DM.Map Instrument [(TE.Text, AbsFile)]] -> DM.Map Instrument [(TE.Text, AbsFile)]
        mergeMaps (m:ms) = Prelude.foldl (\m1 m2 ->
            DMML.merge preserveMissing preserveMissing (zipWithMatched (\k x y -> x ++ y)) m1 m2
            ) m ms

type PdfFile = AbsFile
catpdfs :: BuildEnv -> Book () -> IO (Book PdfFile)
catpdfs env bk = do
    let tf = book_d env
    let fname = "book_" ++ (show $ key $ instrument bk)  ++ ".pdf"
    case (parseRelFile fname) of
        Just f -> do
            let path = tf </> f
            let pdfpaths = Prelude.map toFilePath $ snd $ Prelude.unzip $ DM.toList $ songs bk
            callProcess ("pdftk") (pdfpaths ++ ["cat", "output", toFilePath path])
            -- TEIO.writeFile (toFilePath path) (dat bk)
            pure (path <$ bk)
        _ -> throw (IllegalPDFFile (TE.pack fname))

-- maketex :: BuildEnv ->  Book () -> Book TE.Text
-- maketex env bk = (cct [header, tunes, raw_tex_footer]) <$ bk where
--     cct :: [TE.Text] -> TE.Text
--     cct ts = Prelude.foldl append "" ts

--     header :: TE.Text
--     header = TE.replace "RELEASE_NAME_MARKER_STRING" (TE.pack $ release_name env)
--         (TE.replace "INSTRUMENT_KEY_MARKER_STRING" (TE.pack $ show $ key $ instrument bk) raw_tex_header)

--     tunes = cct $ Prelude.map tune $ DM.toList $ songs bk

--     tune :: (TE.Text, AbsFile) -> TE.Text
--     tune (name, path) =
--         TE.replace "CHART_PDF_MARKER_STRING" (TE.pack $ show $ path) $
--         TE.replace "CHART_TITLE_MARKER_STRING" name raw_tex_tune

-- type TexFile = AbsFile
-- writetex :: BuildEnv -> Book TE.Text -> IO (Book TexFile)
-- writetex env bk = do
--     let tf = tex_d env
--     let fname = "book_" ++ (show $ key $ instrument bk)  ++ ".tex"
--     case (parseRelFile fname) of
--         Just f -> do
--             let path = tf </> f
--             TEIO.writeFile (toFilePath path) (dat bk)
--             pure (path <$ bk)
--         _ -> throw (IllegalPDFFile (TE.pack fname))



-- pdftex :: BuildEnv ->  Book AbsFile -> IO (Book AbsFile)
-- pdftex env bk = let unreplaced = (book_d env) </> (filename . dat bk) in
--     case replaceExtension ".pdf" unreplaced of
--         Just path -> do
--             callProcess (toFilePath $ "xelatex") ["-output-directory", tex_d env, show $ (dat bk)]
--             pure (path <$ bk)
--         _ -> throw (IllegalPDFFile (TE.pack $ show unreplaced))


-- raw_tex_tune :: TE.Text
-- raw_tex_tune = TE.pack $ [r|
-- % CHART_TITLE_MARKER_STRING
-- \chart{CHART_TITLE_MARKER_STRING}
-- \includepdf[pages=-, pagecommand={\thispagestyle{plain}}]{CHART_PDF_MARKER_STRING}
-- |]

-- -- # Header and footer for the tex file that will become our books
-- raw_tex_header :: TE.Text
-- raw_tex_header = TE.pack $ [r|%!TEX encoding = UTF-8 Unicode
-- \documentclass{scrartcl}
-- \usepackage{pdfpages}
-- \usepackage{hyperref}
-- \usepackage{fontspec}
-- \usepackage{tocloft}    % tocloft for table of contents style
-- \usepackage[compact]{titlesec}  % titlesec for title section layout
-- \usepackage{multicol}
-- \usepackage{fancyhdr}
-- \usepackage{geometry}
-- \usepackage{graphicx}
-- \usepackage{tocstyle}

-- \setmainfont[Ligatures={Common,TeX}, Mapping=tex-ansi]{MuseJazzText}

-- % Add a wrapper command for adding a chart, which adds it to the ToC
-- \newcommand{\chart}[1]{%
-- \par\refstepcounter{section} % Increase section counter
-- \sectionmark{#1} % Add section mark (header)
-- \addcontentsline{toc}{section}{\protect\numberline{\thesection} #1}% Add section to ToC
-- % Add more content here, if needed.
-- }

-- % Tighten up the spacing on the ToC
-- \setcounter{tocdepth}{1}
-- \newtocstyle{compact}{%
--   \settocfeature[1]{entryhook}{\bfseries}%
--   \settocfeature[1]{entryvskip}{0pt plus 2pt}%
--   \settocfeature[1]{leaders}{\hfill}%
-- }
-- \usetocstyle{compact}

-- % Add page numbers, even though we're importing pdfs, and move them to the corner
-- \fancyhf{} % clear all header and footers
-- \renewcommand{\headrulewidth}{0pt} % remove the header rule
-- \fancyfoot[LE,RO]{\thepage} % Left side on Even pages; Right side on Odd pages
-- \pagestyle{fancy}
-- \fancypagestyle{plain}{%
--   \fancyhf{}%
--   \renewcommand{\headrulewidth}{0pt}%
--   \fancyhf[lef,rof]{\thepage}%
-- }
-- \geometry{footskip=114pt} % don't set this manually else geometry won't know!


-- \begin{document}

-- % Use the fancy Stompin' At Summerhall logo for the title of the books
-- \title{
--     \vspace{-3em}
--     \includegraphics[width=\textwidth]{resources/logo_bars.pdf}
-- }

-- % (ab)use the author and date command(s) to set the key and release name
-- \author{\vspace{-2em}\Huge INSTRUMENT_KEY_MARKER_STRING}
-- \date{\small Release "RELEASE_NAME_MARKER_STRING"}

-- \maketitle

-- \let\cleardoublepage\clearpage

-- \makeatletter
-- \chapter{
-- \@mkboth{%
-- \MakeUppercase\contentsname}{\MakeUppercase\contentsname}}
-- \begin{multicols*}{2}
-- \@starttoc{toc}
-- \end{multicols*}
-- \makeatother

--  \clearpage
-- |]

-- raw_tex_footer :: TE.Text
-- raw_tex_footer = TE.pack $ [r|\end{document}|]
