{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Yaml as Y
import qualified Data.ByteString as BS
import Data.HashMap.Lazy as M hiding (map)
import Data.Scientific (FPFormat(Generic), formatScientific)
import Data.Text (Text(..), unpack, pack, intercalate, take, length)
import qualified Data.Text as T
import Data.Vector as V hiding (map)
import System.Console.CmdArgs
import System.Exit (die, exitFailure, exitSuccess, ExitCode(..))
import Text.XML.Light
import Text.Printf (printf)
    
toSingular :: Text -> Text
toSingular "women" = "woman"
toSingular "men" = "man"
toSingular "fish" = "aFish"
toSingular "data" = "datum"
toSingular x = T.take (T.length x - 1) x


toEl :: (Text, Y.Value) -> Text
-- create elements from Array.  The element tag is derived from singular from of surrounding tag, derived from Array key.
toEl (k, (Y.Array a)) =
    let k' = unpack k
        k'' = unpack $ toSingular k
        cs = intercalate "" $ map (\v -> pack $ printf "<%s>%s</%s>\n" k'' (toXML $ Just v) k'') $ V.toList a
    in intercalate "" $ [pack $ printf "<%s>\n" k', cs, pack $ printf "</%s>\n"  k']
toEl (k, v) = let k' = unpack k 
              in intercalate "" [pack $ printf "<%s>" k', toXML $ Just v, pack $ printf "</%s>\n"  k']

-- translate yaml data type to an XML element.  Arrays require special treatment because they need each element to have a name.
toXML :: Maybe Y.Value -> Text
toXML (Just (Y.String s)) = s
toXML (Just (Y.Number n)) = pack $ formatScientific Generic Nothing n
toXML (Just (Y.Bool True)) = "true"
toXML (Just (Y.Bool False)) = "false"
toXML (Just (Y.Null)) = ""
toXML Nothing = pack "nothing"

toXML (Just (Y.Object m)) = intercalate "" $ map toEl (M.toList m :: [(Text, Y.Value)])
toXML (Just (Y.Array a)) = intercalate "\n" $ map (\v -> toXML $ Just v) $ V.toList a
toXML _ = pack "unknown YAML element!"

data Options = Options {
      opt_reverse :: Bool
      , opt_dump :: Bool
      , opt_files :: [String]
} deriving (Data, Typeable, Show)
 
options :: Options
options = Options { 
            opt_reverse = False &= typ "Bool" &= help "reverse direction of conversion?"  &= name "reverse"
          , opt_dump = False &= typ "Bool" &= help "dump raw yaml"  &= name "dump"
          , opt_files = [] &= args &= typFile
          }
          &= summary "csv2dsvr v0.1, (C) John Noll 2016"
          &= program "greeter"
main :: IO ()
main = do
  opts <- cmdArgs options

  c <- BS.getContents
  if opt_dump opts then
      print $ (Y.decode c :: Maybe Y.Value)
  else case (Y.decode c :: Maybe Y.Value) of
       Just y -> do { putStrLn "<doc>"; putStrLn $ unpack $ toXML $ Just y; putStrLn "</doc>"; exitSuccess }
       otherwise -> die "(nothing)"

