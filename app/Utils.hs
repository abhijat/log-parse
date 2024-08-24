module Utils where

import Data.Bits (Bits (shiftL), complement, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Void (Void)
import Data.Word (Word32, Word64, Word8)
import Text.Megaparsec (MonadParsec (lookAhead), Parsec, count, eof, skipCount, try, (<|>))
import Text.Megaparsec.Byte (char)
import Text.Megaparsec.Byte.Binary (word32le, word64le, word8)
import Text.Megaparsec.Debug (dbg)

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

nullHeader :: Parser ()
nullHeader = skipCount 61 (char 0)

nullOrEof :: Parser ()
nullOrEof = try $ nullHeader <|> eof

data BatchType
  = BData
  | BRaftConfig
  | BNodeManagementCommand
  | BTopicManagementCommand
  deriving (Show)

batchTypeFrom :: Word8 -> BatchType
batchTypeFrom w =
  case w of
    1 -> BData
    6 -> BTopicManagementCommand
    2 -> BRaftConfig
    17 -> BNodeManagementCommand
    _ -> undefined

data TMC = TMC {cmd :: String} deriving (Show)

data VNode = VNode
  { vNodeId :: Word32,
    vNodeRevision :: Word64
  }
  deriving (Show)

parseVNode :: Parser VNode
parseVNode = parseFromEnvelope $ VNode <$> word32le <*> word64le

data GroupNodes = GroupNodes
  { gnVoters :: [VNode],
    gnLearners :: [VNode]
  }
  deriving (Show)

parseGroupNodes :: Parser GroupNodes
parseGroupNodes =
  parseFromEnvelope $
    GroupNodes
      <$> dbg "voters" (parseVector parseVNode)
      <*> dbg "learners" (parseVector parseVNode)

data RaftConfigUpdate = RaftConfigUpdate
  { rcUpdateReplicasToAdd :: [VNode],
    rcUpdateReplicasToRemove :: [VNode],
    rcLearnerStartOffset :: Maybe Word64
  }
  deriving (Show)

parseRaftConfigUpdate :: Parser RaftConfigUpdate
parseRaftConfigUpdate = parseFromEnvelope go
  where
    go =
      RaftConfigUpdate
        <$> parseVector parseVNode
        <*> parseVector parseVNode
        <*> parseOptional word64le

data RaftConfiguration = RaftConfiguration
  { rcCurrent :: GroupNodes,
    rcConfigUpdate :: Maybe RaftConfigUpdate,
    rcOld :: Maybe GroupNodes,
    rcRevision :: Word64
  }
  deriving (Show)

parseRaftConfiguration :: Parser RaftConfiguration
parseRaftConfiguration =
  dbg "version" (lookAhead word8)
    >> parseFromEnvelope go
  where
    go :: Parser RaftConfiguration
    go =
      RaftConfiguration
        <$> dbg "pgn" parseGroupNodes
        <*> parseOptional parseRaftConfigUpdate
        <*> parseOptional parseGroupNodes
        <*> word64le

data RecordValue
  = RaftConfigCmd RaftConfiguration
  | TopicManagementCommand TMC
  | Data [Word8]
  deriving (Show)

parseTopicManagementCommand :: Parser TMC
parseTopicManagementCommand = undefined

parseRecord :: BatchType -> Parser RecordValue
parseRecord BRaftConfig =
  dbg "size of record value:" parseVarint
    >> RaftConfigCmd <$> parseRaftConfiguration
parseRecord BNodeManagementCommand = undefined
parseRecord BData = Data <$> parseSizePrefixedBytes
parseRecord BTopicManagementCommand = do
  cmdType <- word8
  case cmdType of
    0 -> return $ TopicManagementCommand (TMC "create_topic")
    1 -> return $ TopicManagementCommand (TMC "delete_topic")
    10 -> return $ TopicManagementCommand (TMC "topic_lifecycle_transition")
    2 -> return $ TopicManagementCommand (TMC "update_partitions")
    3 -> return $ TopicManagementCommand (TMC "finish_partitions_update")
    4 -> return $ TopicManagementCommand (TMC "update_topic_properties")
    5 -> return $ TopicManagementCommand (TMC "create_partitions")
    6 -> return $ TopicManagementCommand (TMC "create_non_replicable_topic")
    7 -> return $ TopicManagementCommand (TMC "cancel_moving_partition_replicas")
    11 -> return $ TopicManagementCommand (TMC "force_partition_reconfiguration")
    _ -> undefined

parseTheRecord :: BatchType -> Parser TheRecord
parseTheRecord bType =
  TheRecord
    <$> dbg "recsize" parseVarint
    <*> dbg "attrs" word8
    <*> dbg "tsdelta" parseVarint
    <*> dbg "offdelta" parseVarint
    <*> dbg "key" parseSizePrefixedBytes
    <*> dbg "val" (parseRecord bType)
    <*> dbg "recheaders" parseRecordHeaders

data TheRecord = TheRecord
  { recSize :: Int,
    recAttrs :: Word8,
    recTSDelta :: Int,
    recOffsetDelta :: Int,
    recKey :: [Word8],
    recValue :: RecordValue,
    recHeaders :: [RecordHeader]
  }
  deriving (Show)

data RecordHeader = RecordHeader
  { rhKey :: [Word8],
    rhValue :: [Word8]
  }
  deriving (Show)

parseRecordHeader :: Parser RecordHeader
parseRecordHeader =
  RecordHeader <$> parseSizePrefixedBytes <*> parseSizePrefixedBytes

parseRecordHeaders :: Parser [RecordHeader]
parseRecordHeaders = do
  sz <- parseVarint
  count sz parseRecordHeader

parseFromEnvelope :: Parser a -> Parser a
parseFromEnvelope innerParser = parseEnvelope >> innerParser

data Envelope = Envelope
  { eVersion :: Word8,
    eCompatVersion :: Word8,
    eSize :: Word32
  }
  deriving (Show)

parseEnvelope :: Parser Envelope
parseEnvelope =
  Envelope
    <$> dbg "eversion" word8
    <*> dbg "eversionCompat" word8
    <*> dbg "esize" word32le

parseVector :: (Show a) => Parser a -> Parser [a]
parseVector innerParser = do
  size <- dbg "vecsize" word32le
  count (fromIntegral size) innerParser

parseOptional :: (Show a) => Parser a -> Parser (Maybe a)
parseOptional innerParser = do
  isThere <- word8
  case isThere of
    0 -> return Nothing
    1 -> Just <$> innerParser
    _ -> undefined
