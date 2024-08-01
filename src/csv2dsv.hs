{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module Main where
 
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as DL
import Data.CSV.Conduit 
import Data.Conduit.List as CL
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import System.Console.CmdArgs
import System.IO (stdin, stdout, openFile, IOMode(..), Handle)

data Options = Options {
      opt_reverse :: Bool
      , opt_files :: [String]
} deriving (Data, Typeable, Show)
 
options :: Options
options = Options { 
            opt_reverse = False &= typ "Bool" &= help "reverse direction of conversion?"  &= name "reverse"
          , opt_files = [] &= args &= typFile
          }
          &= summary "csv2dsvr v0.1, (C) John Noll 2016"
          &= program "greeter"
 
csvCfg :: CSVSettings
csvCfg = defCSVSettings

dsvCfg :: CSVSettings
dsvCfg = defCSVSettings { csvSep = '|', csvQuoteChar = Nothing } 

deNewline :: BS.ByteString -> BS.ByteString
deNewline = BS.map (\c -> if c == '\n' || c == '\r' then '\t' else c) 

deNewlineRow :: (Row BS.ByteString) -> (Row BS.ByteString)
deNewlineRow = DL.map deNewline

passthrough :: Monad m => Conduit (Row BS.ByteString) m (Row BS.ByteString)
passthrough = CL.map deNewlineRow

xfrm :: Handle -> Handle -> CSVSettings -> CSVSettings -> IO ()
xfrm inHandle outHandle inCfg outCfg = runResourceT $ CB.sourceHandle inHandle $= intoCSV inCfg $= passthrough $= fromCSV outCfg  $$ CB.sinkHandle outHandle

main :: IO ()
main = do
  opts <- cmdArgs options
  let inCfg = if opt_reverse opts then dsvCfg else csvCfg
      outCfg = if opt_reverse opts then csvCfg else dsvCfg
      
  case DL.length $ opt_files opts of
                   0 -> xfrm stdin stdout  inCfg outCfg
                   1 -> openFile (opt_files opts !!0) ReadMode >>= (\inh -> xfrm inh stdout inCfg outCfg)
                   otherwise -> do
                     inh  <- openFile (opt_files opts !! 0) ReadMode 
                     outh <- openFile (opt_files opts !! 1) WriteMode 
                     xfrm inh outh  inCfg outCfg

