{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.RedisRESP
  (RESP(..),
  Command(..),
  CommandPart(..),
  RedisCommand(..),
  toCommand,
  encode)
where

import qualified Data.ByteString as BS (ByteString, concat, length)
import qualified Data.ByteString.UTF8 as BSU (fromString)
import Data.Int (Int64)
import qualified Data.Map as M (Map, size, toList)
import qualified Data.Set as S (Set, size, toList)
import qualified Data.Text as T (Text)
import qualified Data.Text.Encoding as TSE (encodeUtf8)

data RESP
  = String T.Text
  | Error T.Text
  | Integer Int64
  | ByteString BS.ByteString
  | Array [RESP]
  | Null
  | NullByteString
  | Boolean Bool
  | ByteError BS.ByteString
  | Map (M.Map RESP RESP)
  | Set (S.Set RESP)
  deriving (Eq, Show)

instance Ord RESP where
  compare :: RESP -> RESP -> Ordering
  compare (String t) (String q) = compare t q
  compare (Error t) (Error q) = compare t q
  compare (Integer t) (Integer q) = compare t q
  compare (ByteString t) (ByteString q) = compare t q
  compare Null Null = EQ
  compare (Array t) (Array q) = compare t q
  compare (Boolean t) (Boolean q) = compare t q
  compare (ByteError t) (ByteError q) = compare t q
  compare (Map t) (Map q) = compare t q
  compare (Set t) (Set q) = compare t q
  compare (String _) _ = LT
  compare (Error _) _ = LT
  compare (Integer _) _ = LT
  compare (ByteString _) _ = LT
  compare (Array _) _ = LT
  compare Null _ = LT
  compare NullByteString _ = LT
  compare (Boolean _) _ = LT
  compare (ByteError _) _ = LT
  compare (Map _) _ = LT

data Command = ECHO | SET | GET | PING | PX | CONFIG

data CommandPart = Command Command | Param RESP

type RedisCommand = [CommandPart]

toCommand :: T.Text -> Maybe Command
toCommand "echo" = Just ECHO
toCommand "set" = Just SET
toCommand "get" = Just GET
toCommand "ping" = Just PING
toCommand "px" = Just PX
toCommand "config" = Just CONFIG
toCommand _ = Nothing

instance Show Command where
  show :: Command -> String
  show ECHO = "echo"
  show SET = "set"
  show GET = "get"
  show PING = "ping"
  show PX = "px"
  show CONFIG = "config"

encode :: RESP -> BS.ByteString
encode (String t) = "+" <> TSE.encodeUtf8 t <> "\r\n"
encode (Error t) = "-" <> TSE.encodeUtf8 t <> "\r\n"
encode (Integer n) = ":" <> BSU.fromString (show n) <> "\r\n"
encode Null = "_\r\n"
encode (Boolean True) = "#t\r\n"
encode (Boolean False) = "#f\r\n"
encode (ByteString bs) = "$" <> (BSU.fromString . show . BS.length) bs <> "\r\n" <> bs <> "\r\n"
encode (ByteError bs) = "!" <> (BSU.fromString . show . BS.length) bs <> "\r\n" <> bs <> "\r\n"
encode (Array xs) = "*" <> (BSU.fromString . show . length) xs <> "\r\n" <> BS.concat (fmap encode xs)
encode (Map map) = "%" <> (BSU.fromString . show . M.size) map <> "\r\n" <> BS.concat ((\(x, y) -> encode x <> encode y) <$> M.toList map)
encode (Set set) = "~" <> (BSU.fromString . show . S.size) set <> "\r\n" <> BS.concat (encode <$> S.toList set)
encode NullByteString = "$-1\r\n"