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

data YConfig = YConfig
               { optVerbose     :: Bool
               , optVersion     :: Bool
               , optHelp        :: Bool
               , identifier     :: String
               , title          :: String
               , author         :: String
               , inputDir       :: Maybe FilePath
               , outputDir      :: Maybe FilePath
               , contentsDir    :: FilePath
               , pagesExt       :: String
               , spine          :: [String]
               , nav            :: String
               } deriving Show

getConfig :: IO (Maybe YConfig)
getConfig = decodeFile "config.yml"


parseYamlYConfig :: Value -> Parser YConfig
parseYamlYConfig (Object o) = do
  verb <- o .:? "verbose"      .!= False
  idnt <- o .:? "identifier"   .!= ""
  titl <- o .:? "title"        .!= ""
  auth <- o .:? "author"       .!= "no-author"
  inpd <- o .:? "input-dir"
  outd <- o .:? "output-file"
  cDir <- o .:? "contents-dir" .!= "."
  pExt <- o .:? "pages-ext"    .!= "xhtml"
  spn  <- o .:  "spine"
  nav  <- o .:  "nav"
  return $ YConfig verb False False idnt titl auth inpd outd cDir pExt spn nav


instance FromJSON YConfig where
  parseJSON = parseYamlYConfig
