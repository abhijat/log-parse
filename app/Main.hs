module Main where

import qualified Data.ByteString as BS
import Data.Void (Void)
import Header (Header (hBatchType), hRecordCount, parseHeader)
import Text.Megaparsec (ParseErrorBundle (bundleErrors), count, manyTill, runParser)
import Text.Megaparsec.Debug (dbg)
import Utils (Parser, TheRecord, batchTypeFrom, nullOrEof, parseTheRecord)

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Batch = Batch
  { batchHeader :: Header,
    batchRecords :: [TheRecord]
  }

instance Show Batch where
  show (Batch h recs) = show h <> " (" <> show (length recs) <> " records)"

parseBatch :: Parser Batch
parseBatch = do
  hdr <- dbg "header" parseHeader
  records <- count (fromIntegral $ hRecordCount hdr) (parseTheRecord (batchTypeFrom $ hBatchType hdr))
  return $ Batch hdr records

run :: String -> IO ()
run fpath = do
  f <- BS.readFile fpath
  let parseResults = runParser (manyTill parseBatch nullOrEof) "" f
  -- let parseResults = runParser (count 1 parseBatch) "" f
  either analyzeBundle showResults parseResults
  where
    analyzeBundle :: ParseErrorBundle BS.ByteString Void -> IO ()
    analyzeBundle b =
      print $ bundleErrors b
    showResults = mapM_ print
