{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Data.RedisRESP where

import qualified Data.ByteString as BS (ByteString, length, concat)
import qualified Data.ByteString.UTF8 as BSU ( fromString )
import Data.Int (Int64)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
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
  deriving Eq

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

data Command = ECHO | SET | GET | PING

data CommandPart = Command Command | Param RESP

type RedisCommand = [CommandPart]

argNum :: Command -> Int
argNum ECHO = 1
argNum SET = 1
argNum GET = 0
argNum PING = 0

toCommand :: T.Text -> Command
toCommand "echo" = ECHO
toCommand "set" = SET
toCommand "get" = GET
toCommand "ping" = PING
toCommand _ = error "unexpected"

instance Show Command where
  show :: Command -> String
  show ECHO = "echo"
  show SET = "set"
  show GET = "get"
  show PING = "ping"

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
encode (Map map) = "%" <> (BSU.fromString . show . M.size) map <> "\r\n" <> BS.concat ((\(x,y) -> encode x <> encode y) <$> M.toList map)
encode (Set set) = "~" <> (BSU.fromString . show . S.size) set <> "\r\n" <> BS.concat (encode <$> S.toList set)
encode NullByteString = "$-1\r\n"