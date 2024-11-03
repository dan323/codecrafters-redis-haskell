{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Simple.TCP (serve, HostPreference(HostAny), closeSock, recv, send)
import qualified Data.ByteString as BS
import Control.Monad (void, forever)


main :: IO ()
main = do
    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    -- putStrLn "Logs from your program will appear here"

    -- Uncomment this block to pass stage 1
    let port = "6379"
    putStrLn $ "Redis server listening on port " ++ port
    forever $ do
        serve HostAny port $ \(socket, address) -> do
            input <- recv socket 16
            case input of
                Just x -> void $ send socket "+PONG\r\n"
                Nothing -> return ()
            putStrLn $ "successfully connected client: " ++ show address
            closeSock socket
