{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever, void)
import qualified Data.ByteString as BS (ByteString, putStr)
import Data.ByteString.Char8 as BS (readInteger)
import Data.RedisRESP (RESP(..), encode, RedisCommand(..), CommandPart(..), Command(..), toCommand)
import Data.Text (toLower)
import qualified Data.Text.Encoding as TSE (decodeUtf8)
import Network.Simple.TCP (HostPreference (..), recv, send, serve, Socket, SockAddr)
import Text.Megaparsec (errorBundlePretty, parse)
import Text.Parser (respParser)
import qualified Data.Map as M (Map, empty, insert, delete, lookup)
import Control.Monad.State (StateT, gets, modify, evalStateT, liftIO, get, lift, put)
import Data.Functor (($>))
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime, NominalDiffTime)

-- Type alias for the variable store
type VarStore = M.Map BS.ByteString (BS.ByteString, Maybe UTCTime)

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
    date <- lift getCurrentTime
    case input of
        Just x -> (interpret date . toCommandAndParams . runParser) x >>= (liftIO . send socket . encode)
        Nothing -> return ()

runParser :: BS.ByteString -> RESP
runParser input = case parse respParser "" input of
  Left x -> error $ errorBundlePretty x
  Right y -> y

toCommandAndParams :: RESP -> RedisCommand
toCommandAndParams (Array (x:xs)) = maybe (Param x) Command (transformToCommand x): toCommandAndParams (Array xs)
    where
        transformToCommand (ByteString p) = (toCommand . toLower . TSE.decodeUtf8) p
        transformToCommand _ = Nothing
toCommandAndParams (Array []) = []
toCommandAndParams t = error $ show t

interpret :: UTCTime -> RedisCommand -> ServerState RESP
interpret _ [Command SET, Param (ByteString key), Param (ByteString value)] = modify (M.insert key (value, Nothing)) $> String "OK"
interpret date [Command SET, Param (ByteString key), Param (ByteString value), Command PX, Param (ByteString time)] = do -- modify (M.insert key (value, Nothing)) $> String "OK"
    let milisecs = maybe (error "Unable to read integer") fst (BS.readInteger time)
    lift . putStr $ show milisecs
    let expireDate = addUTCTime ((realToFrac :: Double -> NominalDiffTime)((fromInteger :: Integer -> Double) milisecs/(1000 :: Double))) date
    lift . putStr $ show expireDate
    modify (M.insert key (value, Just expireDate)) $> String "OK"
interpret date [Command GET, Param (ByteString key)] = do -- gets (maybe NullByteString (ByteString . fst) . M.lookup key)
    map <- get
    let mvalue = M.lookup key map
    case mvalue of
        Nothing -> error "Data was not inserted" -- return NullByteString
        Just (v, mTime) -> case mTime of
                             Nothing -> return (ByteString v)
                             Just t -> case compare t date of
                                        LT -> return (ByteString v)
                                        _ -> put (M.delete key map) $> error ("Data was deleted " ++ show date ++ " " ++ show t ++ " " ++ show (compare t date))
interpret _ [Command PING] = return $ String "PONG"
interpret _ [Command ECHO, Param (ByteString b)] = return $ ByteString b
interpret _ _ = error "Unexpected"