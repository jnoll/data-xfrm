module Main where
import Text.Table.XMLTable (getXML, getXML')
import Data.List (intercalate)

main :: IO ()
main = do
  c <- getContents
  let (h,rs) = getXML' c
  putStrLn $ intercalate "|" h
  putStrLn $ intercalate "\n" $ map (\r -> intercalate "|" r) rs
