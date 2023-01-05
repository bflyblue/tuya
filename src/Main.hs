{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (forever, when)
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types
import Crypto.Error
import Crypto.Hash
import Data.ByteArray
import Data.ByteString as BS
import Data.Serialize.Get
import Data.Word
import Network.Socket
import Network.Socket.ByteString

main :: IO ()
main = do
  s <- socket AF_INET Datagram defaultProtocol
  bind s (SockAddrInet 6667 (tupleToHostAddress (0, 0, 0, 0)))
  -- let (Right key) = Base16.decode "face2953b6fc7118"
  let key :: Digest MD5
      key = hash ("yGAdlopoPVldABfn" :: ByteString)
  -- face2953b6fc7118
  -- 8739eb64e339db03

  forever $ do
    (b, f) <- recvFrom s 65536
    let t = either error id (decodeTuya b)
    -- case f of
    --   SockAddrInet _ ha
    --     | ha == tupleToHostAddress (192, 168, 2, 26) -> print (f, t, decryptPayload key (payload t))
    --   _ ->
    --     print f
    when (prefix t == 0x55aa && suffix t == 0xaa55) $
      print (f, decryptPayload (convert key) (payload t))

data TuyaPacket = TuyaPacket
  { prefix :: Word32
  , sequence' :: Word32
  , command :: Word32
  , payloadSize :: Word32
  , returnCode :: Word32
  , payload :: ByteString
  , crc :: Word32
  , suffix :: Word32
  }
  deriving (Show)

decodeTuya :: ByteString -> Either String TuyaPacket
decodeTuya = runGet getTuyaPacket

getTuyaPacket :: Get TuyaPacket
getTuyaPacket = do
  prefix <- getWord32be
  sequence' <- getWord32be
  command <- getWord32be
  payloadSize <- getWord32be
  returnCode <- getWord32be
  payload <- getByteString (fromIntegral payloadSize - 12)
  crc <- getWord32be
  suffix <- getWord32be
  return TuyaPacket{..}

data CommandType
  = Udp
  | ApConfig
  | Active
  | SessKeyNegStart
  | SessKeyNegRes
  | SessKeyNegFinish
  | Unbind
  | Control
  | Status
  | HeartBeat
  | DpQuery
  | QueryWifi
  | TokenBind
  | ControlNew
  | EnableWifi
  | DpQueryNew
  | SceneExecute
  | UdpNew
  | ApConfigNew
  | BroadcastLpv34
  | LanExtStream
  | LanGwActive
  | LanSubDevRequest
  | LanDeleteSubDev
  | LanReportSubDev
  | LanScene
  | LanPublishCloudConfig
  | LanPublicAppConfig
  | LanExportAppConfig
  | LanPublishScenePanel
  | LanRemoveGw
  | LanCheckGwUpdate
  | LanGwUpdate
  | LanSetGwChannel
  deriving (Show, Eq, Ord)

instance Enum CommandType where
  fromEnum :: CommandType -> Int
  fromEnum Udp = 0
  fromEnum ApConfig = 1
  fromEnum Active = 2
  fromEnum SessKeyNegStart = 3
  fromEnum SessKeyNegRes = 4
  fromEnum SessKeyNegFinish = 5
  fromEnum Unbind = 6
  fromEnum Control = 7
  fromEnum Status = 8
  fromEnum HeartBeat = 9
  fromEnum DpQuery = 10
  fromEnum QueryWifi = 100
  fromEnum TokenBind = 12
  fromEnum ControlNew = 13
  fromEnum EnableWifi = 14
  fromEnum DpQueryNew = 16
  fromEnum SceneExecute = 17
  fromEnum UdpNew = 19
  fromEnum ApConfigNew = 20
  fromEnum BroadcastLpv34 = 35
  fromEnum LanExtStream = 40
  fromEnum LanGwActive = 240
  fromEnum LanSubDevRequest = 241
  fromEnum LanDeleteSubDev = 242
  fromEnum LanReportSubDev = 243
  fromEnum LanScene = 244
  fromEnum LanPublishCloudConfig = 245
  fromEnum LanPublicAppConfig = 246
  fromEnum LanExportAppConfig = 247
  fromEnum LanPublishScenePanel = 248
  fromEnum LanRemoveGw = 249
  fromEnum LanCheckGwUpdate = 250
  fromEnum LanGwUpdate = 251
  fromEnum LanSetGwChannel = 252

decryptPayload :: ByteString -> ByteString -> ByteString
decryptPayload key bs = ecbDecrypt cipher bs -- \$ pad (ZERO (blockSize cipher)) bs
 where
  cipher :: AES128
  cipher = throwCryptoError $ cipherInit key