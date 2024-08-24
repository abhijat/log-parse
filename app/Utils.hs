module Utils where

import Data.Bits (Bits (shiftL), complement, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec (Parsec, count)
import Text.Megaparsec.Byte.Binary (word8)

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

type Parser = Parsec Void BS.ByteString
