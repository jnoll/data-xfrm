module Main where
import Text.Table.XMLTable (putXML)
import Data.List (intercalate)
import Text.XML.Light

main :: IO ()
main = do
  putStrLn $ ppContent $ putXML (["a", "b", "c"], [["A", "B", "C"], ["Q", "R", "X"]])
