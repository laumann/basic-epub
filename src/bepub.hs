-- | BePub: Basic ePub handling tool.

import System.Environment (getProgName, getArgs)

import qualified EpubPack as EpubPack
import Options
import Config
import MetaInf
import Data.Maybe (fromMaybe)

type Command = String

-- | What kind of usage do I want?
-- Scenario 0: $ bepub pack dir [-o outfile]  # pack a directory (just zip it)
-- Scenario 1: $ bepub init dir               # initialise an .epub layout
-- Scenario 2: $ bepub ...

-- First argument must be a command OR --help | --version
main = do
    args <- getArgs
    case args of 
      []          -> printUsage
      (cmd:args') -> case getOpts args' of
        (opts, nonOpts, []) -> if optVersion opts
                               then printVersion
                               else run cmd (opts, nonOpts)
        (_, _, errs)        -> do putStrLn $ concat errs
                                  printUsage

run :: Command -> (Options,[String]) -> IO ()
run "pack" (options,unmatched) = EpubPack.pack options unmatched
run cmd _
  | cmd `elem` ["-h", "--help"]    = printUsage
  | cmd `elem` ["-V", "--version"] = printVersion
  | otherwise                      = if (head cmd) == '-'
                                     then do putStrLn $ "Unknown option: '" ++ cmd ++ "'"
                                             printUsage
                                     else putStrLn $ "bepub: '" ++ cmd ++ "' is not a recognized command. See 'bepub --help'"

printUsage = do prog <- getProgName
                putStr $ usageInfo prog

printVersion = do prog <- getProgName
                  putStr $ prog ++ " version whatever."