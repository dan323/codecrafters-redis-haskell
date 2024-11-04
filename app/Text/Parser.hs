{-# LANGUAGE OverloadedStrings #-}

module Text.Parser where

import Data.RedisRESP ( RESP(..) )
import Data.Word ( Word8 )
import Data.ByteString as BS ( ByteString, pack )
import Data.ByteString.Char8 as BS ( readInteger )
import Data.Void ( Void )
import Data.Bifunctor (first)
import Text.Megaparsec ( Parsec, manyTill, try, satisfy )
import Text.Megaparsec.Byte ( char, asciiChar, string )
import Data.Char ( ord )
import Control.Applicative ( (<|>), optional )
import Data.Text.Encoding ( decodeASCII )
import Data.Word8 ( _colon, _hyphen, _plus, _0, _9 )
import Data.Maybe (fromMaybe)

type RESPParser = Parsec Void BS.ByteString RESP

stringParser :: RESPParser
stringParser = String . decodeASCII. BS.pack <$> (char _plus *> manyTill asciiChar (string "\r\n"))

errorParser :: RESPParser
errorParser = Error . decodeASCII. BS.pack <$> (char _hyphen *> manyTill asciiChar (string "\r\n"))

intParser :: RESPParser
intParser = Integer . maybe 0 (fst . first fromInteger) . BS.readInteger . BS.pack <$> (char _colon *> optional (try (char _plus) <|> char _hyphen) >>= (\k -> maybe id (:) k <$> (anyTill (satisfy isDigit) (string "\r\n")))

isDigit :: Word8 -> Bool
isDigit w = (_0 <= w) && (w <= _9)