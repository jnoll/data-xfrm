module Main where
import Text.Table.XMLTable (putXML, putXML')
import Text.Table.CSVTable (getCSV)
import Data.List (intercalate)
import Text.XML.Light

main :: IO ()
main = do
  c <- getContents
  let (h, rs) = getCSV c
  putStrLn $ ppcContent (useExtraWhiteSpace True prettyConfigPP) $ putXML' (Just "root") (h, rs)
