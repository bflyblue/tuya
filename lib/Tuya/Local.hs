module Tuya.Local where

import Data.ByteString
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict')
import Data.IORef
import Tuya.Decode
import Tuya.Encode
import Tuya.Types

connect :: S.SockAddr -> Protocol -> ByteString -> IO Client
connect sockaddr protocol key = do
  s <- S.socket S.AF_INET S.Stream S.defaultProtocol
  S.connect s sockaddr
  seqno <- newIORef 0
  return (Client s protocol key seqno)

close :: Client -> IO ()
close c = S.close (clientSocket c)

send :: Client -> ByteString -> IO ()
send c payload = do
  S.sendAll (clientSocket c) payload

recv :: Client -> Int -> IO ByteString
recv c = S.recv (clientSocket c)

sendCmd :: ToJSON a => Client -> CommandType -> a -> IO ()
sendCmd c cmd payload = do
  seqno <- atomicModifyIORef' (clientSequenceNumber c) (\x -> (x + 1, x))
  let msg = Msg seqno cmd 0 payload
  send c (encode (clientProtocol c) (clientLocalKey c) msg)

recvBS :: Client -> IO (Either String (Msg ByteString))
recvBS c = do
  p <- recv c 65536
  return $ decode (clientLocalKey c) p

recvMsg :: FromJSON a => Client -> IO (Either String (Msg a))
recvMsg c = do
  bs <- recvBS c
  return $
    case bs of
      Right m ->
        case eitherDecodeStrict' (msgPayload m) of
          Right v -> Right m{msgPayload = v}
          Left e -> Left e
      Left e -> Left e
