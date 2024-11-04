
module Data.RedisRESP where

import qualified Data.Text as T
import Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Int (Int64)


data RESP = String T.Text
    | Error T.Text 
    | Integer Int64
    | ByteString BS.ByteString
    | Array [RESP]
    | Null
    | Boolean Bool
    | Double Double
    | BigNumber Integer
    | ByteError BS.ByteString
    | Map (M.Map RESP RESP)
    | Set (S.Set RESP)

newtype RedisCommand = Command T.Text

instance Show RedisCommand where
    show (Command c) = T.unpack $ T.toLower c