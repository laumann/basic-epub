module PrettyXML where
{-
  Given an XML specification (of _some_ kind) output a pretty printed 
  (indented!) format (in a ByteString?).

  TODO: Consider better XML representation...
-}
import Text.PrettyPrint.HughesPJ
import Data.List (intercalate)

exContainer = xmlElm "container" attrs [rootfiles]
  where attrs = attr [ ("xmlns", "urn:oasis:names:tc:opendocument:xmlns:container")
                     , ("version", "1.0")
                     ]
        rootfiles = xmlElm "rootfiles" noAttr [rootfile]
        rootfile  = xmlEmptyElm "rootfile" rootfileAttr
        rootfileAttr = attr [ ("full-path", "book.opf")
                            , ("media-type", "application/oebps-package+xml")
                            ]

data XML = XTag String Attr [XML]
         | XContent String
         | XTagE String Attr
         deriving Show

data Attr = Attr [(String, String)] deriving Show

xmlElm :: String -> Attr -> [XML] -> XML
xmlElm tag attr children = XTag tag attr children

xmlEmptyElm :: String -> Attr -> XML
xmlEmptyElm tag attr = XTagE tag attr

noAttr = attr []

attr :: [(String, String)] -> Attr
attr = Attr 

xmlDoc :: XML -> Doc
xmlDoc xml = renderXml 0 xml

renderXml :: Int -> XML -> Doc
renderXml ind (XTag tag (Attr attr) tags) =
  vcat $ map (nest ind) [ hcat $ concat [ [char '<', text tag, if null attr then empty else char ' ']
                                        , renderAttrs attr 
                                        , [text ">"]
                                        ]
                        , vcat $ (map $ renderXml 2) tags
                        , hcat $ (map text) ["</", tag, ">"]
                        ]
renderXml ind (XTagE tag (Attr attr)) =  hcat $ map (nest ind) $ [char '<', text tag, if null attr then empty else char ' '] ++ renderAttrs attr ++ [text " />"]
renderXml ind (XContent content) = (nest ind) . text $ content

renderAttrs :: [(String, String)] -> [Doc]
renderAttrs attr = intercalate [(char ' ')] $ map (\(k,v) -> renderAttr k v) attr

renderAttr key val = [text key, char '=', doubleQuotes . text $ val]