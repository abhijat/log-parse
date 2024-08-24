module Header where

import Data.Word (Word16, Word32, Word64, Word8)
import Text.Megaparsec.Byte.Binary (word16le, word32le, word64le, word8)
import Utils (Parser)

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
