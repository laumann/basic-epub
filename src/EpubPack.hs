module EpubPack
       ( pack
       , unpack
       , packUsage
       ) where

import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as B
import System.Directory
import System.FilePath
import Options
import Data.Maybe (fromMaybe)
import Text.Printf
import Control.Monad (when)

--------------------------------------------------------------------------------
-- EpubPack: is module handles (un)packing .epub archives
--------------------------------------------------------------------------------

-- it is an error if input path is not specified
-- If -i is not specified then 'unmatched' must contain at least one entry...
-- Also option switches take precedence...
pack :: Config -> [String] -> IO ()
pack options unmatched =
  let (input, output) = case optInput options of Just i -> case optOutput options of
                                                   Just o  -> (i, o)
                                                   Nothing -> (i, (takeBaseName i) ++ ".epub")
                                                 Nothing -> case unmatched of
                                                   []      -> error "No input file specified..."
                                                   [i]     -> (i, (takeBaseName i) ++ ".epub")
                                                   [i,o]   -> (i, o)
                                                   [i,o,_] -> (i, o)
      verbose = optVerbose options
  in do archive <- packFiles input (OptRecursive:if verbose then [OptVerbose] else [])
        B.writeFile output (fromArchive archive)

-- | IF: 'bepub pack --help' then display help text.
packUsage :: IO ()
packUsage = do
  printf "bepub pack "

unpack :: Config -> [String] -> IO ()
unpack config unmatched =
  let arch    = fromMaybe "" (optInput config)
      dir     = fromMaybe (dropExtension arch) (optOutput config)
      zipOpts = if optVerbose config then [OptVerbose] else []
  in do printf "Unpacking to '%s'\n" dir
        createDirectoryIfMissing False dir
        zipf <- B.readFile arch
        doInDirectory dir $ do extractFilesFromArchive zipOpts (toArchive zipf)

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

packFiles' :: [FilePath] -> IO Archive
packFiles' fs = addFilesToArchive [OptVerbose] emptyArchive fs

packFiles :: FilePath -> [ZipOption] -> IO Archive
packFiles f zipOpts = do
  archive <- doInDirectory f $ do addFilesToArchive zipOpts emptyArchive ["."]
  return $ putMimetypeFirst archive

putMimetypeFirst arch = case findEntryByPath "mimetype" arch of
  Just mt -> let arch' = deleteEntryFromArchive "mimetype" arch
             in addEntryToArchive mt arch'
  Nothing -> error "For some reason there's no mimetype file in your epub archive!"

doInDirectory :: FilePath -> IO a -> IO a
doInDirectory path io = do 
  cwd <- getCurrentDirectory
  setCurrentDirectory path
  a <- io
  setCurrentDirectory cwd
  return a
