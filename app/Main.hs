module Main where

import Data.Bits (Bits (shiftL), (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import Text.Megaparsec (Parsec, parseTest, runParser)
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

data Record = Record deriving (Show)

data Batch = Batch
  { batchHeader :: Header,
    records :: [Record]
  }
  deriving (Show)

parseHeader :: Parser Header
parseHeader =
  Header <$> word32le <*> word32le <*> word64le <*> word8 <*> word32le <*> word16le <*> word32le <*> word64le <*> word64le <*> word64le <*> word16le <*> word32le <*> word32le

parseRecord :: Parser Record
parseRecord = undefined

newtype Varint = Varint {i :: Word64} deriving (Show)

parseVarint :: Parser Varint
parseVarint =
  go 0 0
  where
    shifted input shiftSize =
      let hasLowerBitsSet = input .&. 128 /= 0
       in if hasLowerBitsSet
            then
              (input .&. 0x7f) `shiftL` shiftSize
            else
              input `shiftL` shiftSize
    go :: Int -> Int -> Parser Varint
    go result shiftSize = do
      nextWord <- word8
      let hasLowerBitsSet = nextWord .&. 128 /= 0
          shiftedN = shifted (fromIntegral nextWord) shiftSize
          newRes = result .|. shiftedN
      if hasLowerBitsSet
        then go newRes (shiftSize + 7)
        else
          return $ Varint (fromIntegral newRes)

run :: IO ()
run = do
  f <- loadFile
  let foo = runParser parseHeader "" f
  putStrLn "done!"