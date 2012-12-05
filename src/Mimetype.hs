module Mimetype where

import Data.ByteString.Char8 (pack)
import qualified Data.ByteString as BS

mimetype = "application/ebup+zip"

generateMimetype = BS.writeFile "mimetype" (pack mimetype)