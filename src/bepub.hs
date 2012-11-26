-- | BePub: Basic ePub handling tool.

import System.Environment (getProgName, getArgs)
import System.Console.GetOpt

data Flag = Verbose
          | Version
          | Help
          | Output FilePath
          | Input FilePath
          deriving (Eq, Show)

type Command = String

commands :: [Command]
commands = [ "pack"
           ]

options :: [OptDescr Flag]
options = [ Option ['v'] ["verbose"] (NoArg Verbose) "Output extra information."
          , Option ['V'] ["version"] (NoArg Version) "Display version number and exit."
          , Option ['h'] ["help"]    (NoArg Help) "Display help message and exit."
          , Option ['o'] ["output"]  (ReqArg (\s -> Output s) "FILE") "Set the output file."
          , Option ['i'] ["input"]   (ReqArg (\s -> Input s) "DIR") "Set the input directory."
          ]

-- | What kind of usage do I want?
-- Scenario 0: $ bepub pack dir [-o outfile]  # pack a directory (just zip it)
-- Scenario 1: $ bepub init dir               # initialise an .epub layout
-- Scenario 2: $ bepub ...

-- First argument must be a command OR --help | --version
main = do
    args <- getArgs
    case args of 
      []          -> printUsage
      (cmd:args') -> case getOpt Permute options args of
        (flags, nonOpts, [])   -> if Version `elem` flags || Help `elem` flags
                                  then if Help `elem` flags then printUsage else printVersion
                                  else run cmd flags
        (_, _, errs)           -> do putStrLn $ concat errs
                                     printUsage

run :: Command -> [Flag] -> IO ()
run "pack" flags = putStrLn $ "Running command 'pack' with flags: " ++ show flags
run cmd flags
  | cmd `elem` ["-h", "--help"]    = printUsage
  | cmd `elem` ["-v", "--version"] = printVersion
  | otherwise                      = putStrLn $ "bepub: '" ++ cmd ++ "' is not a recognized command. See 'bepub --help'"


printUsage = do prog <- getProgName
                putStr $ usageInfo ("Usage: " ++ prog ++ " [options]\n\nOptions:") options
  
printVersion = do prog <- getProgName
                  putStr $ "This is " ++ prog ++ " version whatever."