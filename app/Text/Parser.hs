{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Text.Parser where

import Control.Applicative.Combinators (count, optional, (<|>))
import Data.Bifunctor (first)
import Data.ByteString as BS (ByteString, pack)
import Data.ByteString.Char8 as BS (readInteger)
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.RedisRESP (RESP (..))
import qualified Data.Set as Set
import Data.Text.Encoding (decodeASCII)
import Data.Void (Void)
import Data.Word (Word8)
import Data.Word8 (_0, _9, _asterisk, _colon, _dollar, _f, _hyphen, _numbersign, _plus, _t, _underscore, _exclam, _percent, _tilde)
import Text.Megaparsec (Parsec, choice, failure, manyTill, satisfy, takeP, try)
import Text.Megaparsec.Byte (asciiChar, char, string)
import qualified Data.Map as M ( fromList )

type RESPParser = Parsec Void BS.ByteString RESP

stringParser :: RESPParser
stringParser = String . decodeASCII . BS.pack <$> (char _plus *> manyTill asciiChar (string "\r\n"))

errorParser :: RESPParser
errorParser = Error . decodeASCII . BS.pack <$> (char _hyphen *> manyTill asciiChar (string "\r\n"))

intParser :: RESPParser
intParser =
  Integer . fromInteger
    <$> ( char _colon *> optional (try (char _plus) <|> char _hyphen)
            >>= ( \case
                    Just _hyphen -> (\n -> -n) <$> intP
                    _ -> intP
                )
        )

nullParser :: RESPParser
nullParser = Null <$ char _underscore <* string "\r\n"

isDigit :: Word8 -> Bool
isDigit w = (_0 <= w) && (w <= _9)

intP :: Parsec Void BS.ByteString Integer
intP = maybe 0 fst . BS.readInteger . BS.pack <$> manyTill (satisfy isDigit) (string "\r\n")

byteStringParser :: RESPParser
byteStringParser = ByteString <$> (char _dollar *> intP >>= (takeP Nothing . fromInteger)) <* string "\r\n"

byteErrorParser :: RESPParser
byteErrorParser = ByteError <$> (char _exclam *> intP >>= (takeP Nothing . fromInteger)) <* string "\r\n"

arrayParser :: RESPParser
arrayParser = Array <$> (char _asterisk *> (fromInteger <$> intP) >>= flip count respParser)

respParser :: RESPParser
respParser = choice [stringParser, errorParser, intParser, byteStringParser, byteErrorParser, arrayParser, nullParser]

boolParser :: RESPParser
boolParser = Boolean <$> (char _numbersign *> ((True <$ char _t) <|> (False <$ char _f))) <* string "\r\n"

mapParser :: RESPParser
mapParser = Map . M.fromList <$> (char _percent *> (fromInteger <$> intP) >>= flip count (respParser >>= (\p -> (p,) <$> respParser)))

setParser :: RESPParser
setParser = Set . Set.fromList <$> (char _tilde *> (fromInteger <$> intP) >>= flip count respParser)