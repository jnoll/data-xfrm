{- Convert XML into "table" - a list of rows consisting of list of strings -}
module Text.Table.XMLTable (getXML, getXML', putXML, putXML') where
import Text.XML.Light
import Data.Maybe (fromJust, fromMaybe)

defQname :: QName
defQname = QName {qName = "uh oh", qURI = Nothing, qPrefix = Nothing}

qname :: String -> QName
qname s = defQname { qName = s }

getField :: Element -> String -> String 
getField el field =
    map noNewLine $ strContent $ fromMaybe blank_element $ findChild (qname field) el -- findChild descends hierarchy
    where noNewLine '\n' = ' '
          noNewLine c = c
          
mkRow :: [String] -> Element -> [String]
mkRow cols el = map (getField el) cols

-- Retrieve a subtree at 'root' and convert it into a table of rows containing values of specified fields
getXML :: String -> [String]  -> String -> ([String], [[String]])
getXML root cols contents = 
  case parseXMLDoc contents of
    Just doc -> 
        let parent = fromMaybe blank_element $ findElement (qname root ) doc -- dfs for first matching element
            children = elChildren parent  -- get all children that are elements
        in (cols, map (mkRow cols) children)
    Nothing -> (["Error", "Details"], [["Unable to parse contents", contents]])

-- Convert XML into table
getXML' :: String -> ([String], [[String]])
getXML' contents = 
  case parseXMLDoc contents of
    Just doc -> 
        let children = elChildren doc  -- get all children that are elements
            cols = map (qName . elName) $ elChildren $ head children
        in (cols, map (mkRow cols) children)
    Nothing -> (["Error", "Details"], [["Unable to parse contents", contents]])

-- Convert table into XML
mkElement :: (String, String) -> Content
mkElement (header, val) = Elem blank_element { elName = blank_name { qName = header }, elContent = [ Text blank_cdata { cdData = val } ] }

mkXMLRow :: Maybe String -> [String] -> [String] -> Content
mkXMLRow name headings row = Elem blank_element { elName = blank_name { qName = fromMaybe "row" name }, elContent = map mkElement $ zip headings row }

mkNamedRow :: [String] -> [String] -> Content
mkNamedRow (_:headings) (typ:vals) = mkXMLRow (Just typ) headings vals

putXML :: Maybe String -> ([String],[[String]]) -> Content
putXML root (headings, rows) = 
    Elem blank_element { elName = blank_name { qName = fromMaybe "doc" root }, elContent = map (mkXMLRow Nothing headings) rows }

-- use first field to name element
putXML' :: Maybe String -> ([String],[[String]]) -> Content
putXML' root (headings, rows) = 
    Elem blank_element { elName = blank_name { qName = fromMaybe "doc" root }, elContent = map (mkNamedRow headings) rows }
