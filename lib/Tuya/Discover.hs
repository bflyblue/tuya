{-# LANGUAGE OverloadedStrings #-}

module Tuya.Discover where

import Control.Monad (forever)
import Crypto.Hash (Digest, MD5, hash)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Network.Socket
import Network.Socket.ByteString

import Tuya.Decode

discoverKey :: ByteString
discoverKey = convert (md5 "yGAdlopoPVldABfn")
 where
  md5 :: ByteString -> Digest MD5
  md5 = hash

discover :: IO ()
discover = do
  s <- socket AF_INET Datagram defaultProtocol
  bind s (SockAddrInet 6667 (tupleToHostAddress (0, 0, 0, 0)))

  forever $ do
    (b, f) <- recvFrom s 65536
    let t = either error id (decode discoverKey b)
    print (f, t)