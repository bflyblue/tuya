module Tuya.Local where

import Data.ByteString
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S
import System.Timeout (timeout)

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict')
import Data.IORef
import Tuya.Decode
import Tuya.Encode
import Tuya.Types

connect :: Int -> S.SockAddr -> Protocol -> ByteString -> IO (Maybe Client)
connect wait sockaddr protocol key = do
  s <- S.socket S.AF_INET S.Stream S.defaultProtocol
  mc <- timeout wait (S.connect s sockaddr)
  case mc of
    Just () -> do
      seqno <- newIORef 0
      return $ Just (Client s protocol key seqno)
    Nothing ->
      return Nothing

close :: Client -> IO ()
close c = S.close (clientSocket c)

send :: Client -> ByteString -> IO ()
send c payload = do
  S.sendAll (clientSocket c) payload

recv :: Int -> Client -> Int -> IO (Maybe ByteString)
recv wait c nbytes =
  timeout wait (S.recv (clientSocket c) nbytes)

sendCmd :: ToJSON a => Client -> CommandType -> a -> IO ()
sendCmd c cmd payload = do
  seqno <- atomicModifyIORef' (clientSequenceNumber c) (\x -> (x + 1, x))
  let msg = Msg seqno cmd 0 payload
  send c (encode (clientProtocol c) (clientLocalKey c) msg)

recvBS :: Int -> Client -> IO (Either String (Msg ByteString))
recvBS wait c = do
  mp <- recv wait c 65536
  case mp of
    Nothing -> return $ Left "recv timeout"
    Just p -> return $ decode (clientProtocol c) (clientLocalKey c) p

recvMsg :: FromJSON a => Int -> Client -> IO (Either String (Msg a))
recvMsg wait c = do
  bs <- recvBS wait c
  return $
    case bs of
      Right m ->
        case eitherDecodeStrict' (msgPayload m) of
          Right v -> Right m{msgPayload = v}
          Left e -> Left e
      Left e -> Left e
