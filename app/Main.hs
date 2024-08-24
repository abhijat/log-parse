module Main where

import Data.Bits (Bits (shiftL, xor), complement, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import Text.Megaparsec (ParseErrorBundle (bundleErrors), Parsec, count, manyTill, runParser, try, (<?>))
import Text.Megaparsec.Byte (char)
import Text.Megaparsec.Byte.Binary (word16le, word32le, word64le, word8)

main :: IO ()
main = putStrLn "Hello, Haskell!"

type Parser = Parsec Void BS.ByteString

loadFile :: IO BS.ByteString
loadFile = BS.readFile "a.log"

data Header = Header
  { hCrc :: Word32,
    hBatchSize :: Word32,
    hBaseOffset :: Word64,
    hBatchType :: Word8,
    hSecondCrc :: Word32,
    hAttrs :: Word16,
    hDelta :: Word32,
    hFirstTS :: Word64,
    hMaxTS :: Word64,
    hProducerId :: Word64,
    hProducerEpoch :: Word16,
    hBaseSeq :: Word32,
    hRecordCount :: Word32
  }
  deriving (Show)

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

parseHeader :: Parser Header
parseHeader =
  Header
    <$> word32le
    <*> word32le
    <*> word64le
    <*> word8
    <*> word32le
    <*> word16le
    <*> word32le
    <*> word64le
    <*> word64le
    <*> word64le
    <*> word16le
    <*> word32le
    <*> word32le

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

parseSizePrefixedBytes :: Parser [Word8]
parseSizePrefixedBytes = do
  sz <- parseVarint
  count sz word8

parseVarint :: Parser Int
parseVarint = do
  parsedInt <- go 0 0
  return (decodeVarint parsedInt)
  where
    shifted input shiftSize
      | input .&. 128 == 0 =
          input `shiftL` shiftSize
      | otherwise =
          (input .&. 0x7f) `shiftL` shiftSize
    go :: Int -> Int -> Parser Int
    go result shiftSize = do
      nextWord <- word8
      let hasLowerBitsSet = nextWord .&. 128 /= 0
          shiftedN = shifted (fromIntegral nextWord) shiftSize
          newRes = result .|. shiftedN
      if hasLowerBitsSet
        then go newRes (shiftSize + 7)
        else
          return newRes
    decodeVarint x =
      let a = x `shiftR` 1
          b = x .&. 1
          cm = complement b + 1
       in a `xor` cm

nullHeader :: Parser [Word8]
nullHeader = try $ count 61 (char 0)

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
