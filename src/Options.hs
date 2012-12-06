module Options
       ( Config(..)
       , defaultOptions 
       , getOpts
       , Options.usageInfo
       ) where

import System.Console.GetOpt
import Data.Maybe (fromMaybe)

data Config = Config { optVersion :: Bool
                     , optVerbose :: Bool
                     , optHelp    :: Bool
                     , optInput   :: Maybe FilePath
                     , optOutput  :: Maybe FilePath
                     } deriving Show



defaultOptions = Config { optVerbose = False   -- Be verbose?
                        , optVersion = False   -- Show version?
                        , optHelp    = False   -- Show help?
                        , optInput   = Nothing -- Current directory
                        , optOutput  = Nothing -- Name of output file
                        }

options :: [OptDescr (Config -> Config)]
options = [ Option ['v'] ["verbose"] (NoArg (\opt -> opt {optVerbose = True})) "Output extra information."
          , Option ['V'] ["version"] (NoArg (\opt -> opt {optVersion = True})) "Display version number and exit."
          , Option ['h'] ["help"]    (NoArg (\opt -> opt {optHelp = True})) "Display help message and exit."
          , Option ['o'] ["output"]  (ReqArg readOutput "FILE") "Set the output file."
          , Option ['i'] ["input"]   (ReqArg readInput "DIR") "Set the input directory (default: current directory)"
          ]
  where readInput arg opt  = opt { optInput  = Just arg }
        readOutput arg opt = opt { optOutput = Just arg }

getOpts :: [String] -> (Config, [String], [String])
getOpts args = let (flags, nonOpts, errs) = getOpt Permute options args
               in (foldl (flip ($)) defaultOptions flags, nonOpts, errs)

usageInfo :: FilePath -> String
usageInfo prog = System.Console.GetOpt.usageInfo ("Usage: " ++ prog ++ " [options]\n\nOptions:") options