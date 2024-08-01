{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
--import Control.Applicative ((<$>), (<*>))
module Text.Table.CSVTable (getCSV) where
import qualified Data.List as DL
import Text.CSV 


-- convert CSV contents into "table" consisting of a header line and a
-- list of rows.
getCSV :: String -> ([String], [[String]])
getCSV contents = case parseCSV "contents" contents of
    Left s -> (["error"], [[show $ s]])
    Right (head:rows) -> (head, rows)

