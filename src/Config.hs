{-# LANGUAGE OverloadedStrings #-}
module Config (getConfig) where

import Data.Yaml
import Data.Text (pack)
--import qualified Data.ByteString as BS

data ToC = ToC { contentsDir :: Maybe String
               , pagesExt    :: String
               , spine       :: [String]
               } deriving Show

getConfig :: IO (Maybe ToC)
getConfig = decodeFile "config.yml"

parseYamlConfig :: Value -> Parser ToC
parseYamlConfig (Object o) = do
  cDir <- o .:? "contents-dir"
  pExt <- o .:? "pages-ext" .!= "xhtml"
  spn  <- o .:  "spine"
  return $ ToC cDir pExt spn

instance FromJSON ToC where
  parseJSON = parseYamlConfig
