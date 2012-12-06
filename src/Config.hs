{-# LANGUAGE OverloadedStrings #-}
{-
  ABOUT
  -----
  This module handles configuration - be it by command line options, or 
  config file. The options specified on the command line take 
  precedence over those specified in the config file. Furthermore, 
  some options can _only_ be specified on the command line.

  NOTES
  -----
   * The default configuration is inlined in 'parseYamlConfig'
     (.:)  means the value is required
     (.:?) means the value is optional, AND the operator (.=!) means 
           that a default value exists.
   *
-}
module Config (getConfig) where

import Data.Yaml
import Data.Text (pack)
--import qualified Data.ByteString as BS

{-
Configuration options:

input directory
output directory
output filename
-}
data Config = Config { optVerbose  :: Bool
                     , optVersion  :: Bool
                     , optHelp     :: Bool
                     -- , optInput    :: Maybe FilePath
                     -- , optOutput   :: Maybe FilePath
                     , contentsDir :: Maybe FilePath
                     , pagesExt    :: String
                     , spine       :: [String]
                     } deriving Show

getConfig :: IO (Maybe Config)
getConfig = decodeFile "config.yml"


parseYamlConfig :: Value -> Parser Config
parseYamlConfig (Object o) = do
  verb <- o .:? "verbose" .!= False
  vers <- o .:? "version" .!= False
  -- inpd <- o .:? "input-dir" .!= Nothing
  -- outd <- o .:? "output-file" .!= Nothing
  cDir <- o .:? "contents-dir"
  pExt <- o .:? "pages-ext" .!= "xhtml"
  spn  <- o .:  "spine"
  return $ Config verb vers False cDir pExt spn


instance FromJSON Config where
  parseJSON = parseYamlConfig
