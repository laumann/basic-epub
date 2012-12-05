module MetaInf where

import System.Directory
--import Text.XML.HaXml.Combinators
import Text.XML.Generator
import Data.Monoid
import Data.ByteString as BS

import Text.XML.HXT.Core

createMetaInf = createDirectoryIfMissing False "META-INF"


-- | XML generation:
-- We want to generate 'container.xml', which is:
{-
<container ...>
  <rootfiles>
    <roofile ...>
  </rootfiles>
</container>
-}


-- | Text.XML.Generator
genContainer :: (String, String) -> [(String, String)] -> Xml Elem
genContainer (xmlns, version) rootfiles = xelem "container" $ xattrs [xattr "xmlns" xmlns, xattr "version" version] <#> genRootFiles rootfiles

genRootFiles :: [(String, String)] -> Xml Elem
genRootFiles rootfiles = xelem "rootfiles" $ (xelems  . Prelude.map genRootFile) rootfiles

genRootFile :: (String, String) -> Xml Elem
genRootFile (fullPath, mediaType) = xelem "rootfile" $ xattrs [xattr "full-path" fullPath, xattr "media-type" mediaType]

genPersonElem :: (String, String) -> Xml Elem
genPersonElem (name, age) = xelem "person" $ xattr "age" age <#> xtext name

outputXml :: Xml Elem -> IO ()
outputXml xmlElm = BS.putStrLn (xrender $ doc defaultDocInfo xmlElm)

render :: Xml Elem -> ByteString
render xmlElm = xrender $ doc defaultDocInfo xmlElm


-- | Text.XML.HXT.Arrow
