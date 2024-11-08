{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever, void)
import qualified Data.ByteString as BS (ByteString, putStr)
import Data.RedisRESP (RESP(..), encode, RedisCommand(..), CommandPart(..), Command(..), toCommand)
import Data.Text (toLower)
import qualified Data.Text.Encoding as TSE (decodeUtf8)
import Network.Simple.TCP (HostPreference (..), recv, send, serve, Socket, SockAddr)
import Text.Megaparsec (errorBundlePretty, parse)
import Text.Parser (respParser)
import qualified Data.Map as M
import Control.Monad.State (StateT, gets, modify, evalStateT, liftIO)
import Data.Functor (($>))

-- Type alias for the variable store
type VarStore = M.Map BS.ByteString BS.ByteString

-- StateT to manage the VarStore state
type ServerState = StateT VarStore IO

main :: IO ()
main = do
  -- You can use print statements as follows for debugging, they'll be visible when running tests.
  putStrLn "Logs from your program will appear here"

  -- Uncomment this block to pass stage 1
  let port = "6379"
  putStrLn $ "Redis server listening on port " ++ port
  serve (Host "localhost") port $ \(sock, addr) -> evalStateT (requestHandler (sock, addr)) M.empty

requestHandler :: (Socket, SockAddr) -> ServerState ()
requestHandler (socket, address) = forever $ do
    input <- recv socket 1024
    case input of
        Just x -> (interpret . toCommandAndParams . runParser) x >>= (liftIO . BS.putStr . encode)
        Nothing -> error "fail"
    liftIO $ putStrLn $ "successfully connected client: " ++ show address

runParser :: BS.ByteString -> RESP
runParser input = case parse respParser "" input of
  Left x -> error $ errorBundlePretty x
  Right y -> y

toCommandAndParams :: RESP -> RedisCommand
toCommandAndParams (Array (x:xs)) = transformToCommand x: (Param <$> xs)
    where
        transformToCommand (ByteString p) = Command ((toCommand . toLower . TSE.decodeUtf8) p)
toCommandAndParams _ = error "This is unexpected"

interpret :: RedisCommand -> ServerState RESP
interpret [Command SET, Param (ByteString key), Param (ByteString value)] = modify (M.insert key value) $> String "OK"
interpret [Command GET, Param (ByteString key)] = gets (maybe NullByteString ByteString . M.lookup key)
interpret [Command PING] = return $ String "PONG"
interpret [Command ECHO, Param (ByteString b)] = return $ ByteString b
interpret _ = error "Unexpected"