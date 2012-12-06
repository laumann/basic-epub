module EpubPack
       ( pack
       , packUsage
       ) where

import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as B
import System.Directory
import System.FilePath
import Options
import Data.Maybe (fromMaybe)

packFiles' :: [FilePath] -> IO Archive
packFiles' fs = addFilesToArchive [OptVerbose] emptyArchive fs

packFiles :: FilePath -> [ZipOption] -> IO Archive
packFiles f zipOpts = do
  cwd <- getCurrentDirectory
  setCurrentDirectory f
  archive <- addFilesToArchive zipOpts emptyArchive ["."]
  setCurrentDirectory cwd
  return (putMimetypeFirst archive)

-- Stupid hack: make sure the mimetype file is the first entry in the archive
putMimetypeFirst arch = case findEntryByPath "mimetype" arch of
  Just mt -> let arch' = deleteEntryFromArchive "mimetype" arch
             in addEntryToArchive mt arch'
  Nothing -> error "For some reason there's no mimetype file in your epub archive!"

-- it is an error if input path is not specified
-- If -i is not specified then 'unmatched' must contain at least one entry...
-- Also option switches take precedence...
pack :: Config -> [String] -> IO ()
pack options unmatched = do 
  archive <- packFiles input (OptRecursive:if verbose then [OptVerbose] else [])
  B.writeFile output (fromArchive archive)
    where (input, output) = case optInput options of
            Just i -> case optOutput options of 
              Just o  -> (i, o)
              Nothing -> (i, (takeBaseName i) ++ ".epub")
            Nothing -> case unmatched of 
                         []      -> error "No input file specified..."
                         [i]     -> (i, (takeBaseName i) ++ ".epub")
                         [i,o]   -> (i, o)
                         [i,o,_] -> (i, o)
          verbose = optVerbose options

-- | IF: 'bepub pack --help' then display help text.
packUsage :: IO ()
packUsage = undefined
