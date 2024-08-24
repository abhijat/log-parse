module Main where

import qualified Data.ByteString as BS
import Data.Void (Void)
import Data.Word (Word8)
import Header (Header, hRecordCount, nullHeader, parseHeader)
import Text.Megaparsec (ParseErrorBundle (bundleErrors), count, manyTill, runParser)
import Text.Megaparsec.Byte.Binary (word8)
import Utils (Parser, parseSizePrefixedBytes, parseVarint)

main :: IO ()
main = putStrLn "Hello, Haskell!"

loadFile :: IO BS.ByteString
loadFile = BS.readFile "a.log"

data Record = Record
  { recSize :: Int,
    recAttrs :: Word8,
    recTSDelta :: Int,
    recOffsetDelta :: Int,
    recKey :: [Word8],
    recValue :: [Word8],
    recHeaders :: [RecordHeader]
  }

instance Show Record where
  show (Record sz attrs tsD offD k v h) =
    "size "
      <> show sz
      <> " attrs "
      <> show attrs
      <> " ts delta "
      <> show tsD
      <> " offset delta "
      <> show offD
      <> " key size "
      <> show (length k)
      <> " val size "
      <> show (length v)
      <> " n-headers "
      <> show (length h)

data RecordHeader = RecordHeader
  { rhKey :: [Word8],
    rhValue :: [Word8]
  }
  deriving (Show)

data Batch = Batch
  { batchHeader :: Header,
    batchRecords :: [Record]
  }

instance Show Batch where
  show (Batch h recs) = show h <> " (" <> show (length recs) <> " records)"

parseRecord :: Parser Record
parseRecord =
  Record <$> parseVarint <*> word8 <*> parseVarint <*> parseVarint <*> parseSizePrefixedBytes <*> parseSizePrefixedBytes <*> parseRecordHeaders

parseBatch :: Parser Batch
parseBatch = do
  hdr <- parseHeader
  records <- count (fromIntegral $ hRecordCount hdr) parseRecord
  return $ Batch hdr records

parseRecordHeader :: Parser RecordHeader
parseRecordHeader =
  RecordHeader <$> parseSizePrefixedBytes <*> parseSizePrefixedBytes

parseRecordHeaders :: Parser [RecordHeader]
parseRecordHeaders = do
  sz <- parseVarint
  count sz parseRecordHeader

run :: IO ()
run = do
  f <- loadFile
  let parseResults = runParser (manyTill parseBatch nullHeader) "" f
  either analyzeBundle showResults parseResults
  where
    analyzeBundle :: ParseErrorBundle BS.ByteString Void -> IO ()
    analyzeBundle b =
      print $ bundleErrors b
    showResults = mapM_ print
