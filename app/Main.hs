{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Simple.TCP (serve, HostPreference(..), recv, send)
import qualified Data.ByteString as BS
import Control.Monad (void, forever)
import Data.RedisRESP (RESP(..), encode )
import Text.Parser (respParser)
import Text.Megaparsec (parse, errorBundlePretty)
import Data.Text (toLower)
import qualified Data.Text.Encoding as TSE (decodeUtf8)


main :: IO ()
main = do
    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    putStrLn "Logs from your program will appear here"

    -- Uncomment this block to pass stage 1
    let port = "6379"
    putStrLn $ "Redis server listening on port " ++ port
    serve (Host "localhost") port $ \(socket, address) -> forever $ do
            input <- recv socket 1024
            case input of
                Just x -> do
                    let inp = runParser x
                    case inp of
                        ByteString x -> if toLower (TSE.decodeUtf8 x) == "ping" then void $ send socket (encode (String "PONG")) else error "unexpected"
                        Array [ByteString y, x] -> if toLower (TSE.decodeUtf8 y) == "echo" then void $ send socket (encode x) else error "unexpected"
                        _ -> error "unexpected"
                Nothing -> error "fail"
            putStrLn $ "successfully connected client: " ++ show address


runParser :: BS.ByteString -> RESP
runParser input = case parse respParser "" input of
  Left x -> error $ errorBundlePretty x
  Right y -> y
