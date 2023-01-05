{-# LANGUAGE InstanceSigs #-}

module Tuya.Types where

import qualified Data.Aeson as Aeson
import Data.ByteString

-- import qualified Data.HashMap.Strict as HM
import Data.Word

data Raw = Raw
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

data Msg = Msg
  { msgSequence :: Word32
  , msgCommand :: CommandType
  , msgReturnCode :: Word32
  , msgPayload :: Aeson.Value
  }
  deriving (Show)

-- data Discovery = Discovery
--   { discovered :: HM.HashMap ByteString ()
--   }

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

  toEnum :: Int -> CommandType
  toEnum 0 = Udp
  toEnum 1 = ApConfig
  toEnum 2 = Active
  toEnum 3 = SessKeyNegStart
  toEnum 4 = SessKeyNegRes
  toEnum 5 = SessKeyNegFinish
  toEnum 6 = Unbind
  toEnum 7 = Control
  toEnum 8 = Status
  toEnum 9 = HeartBeat
  toEnum 10 = DpQuery
  toEnum 100 = QueryWifi
  toEnum 12 = TokenBind
  toEnum 13 = ControlNew
  toEnum 14 = EnableWifi
  toEnum 16 = DpQueryNew
  toEnum 17 = SceneExecute
  toEnum 19 = UdpNew
  toEnum 20 = ApConfigNew
  toEnum 35 = BroadcastLpv34
  toEnum 40 = LanExtStream
  toEnum 240 = LanGwActive
  toEnum 241 = LanSubDevRequest
  toEnum 242 = LanDeleteSubDev
  toEnum 243 = LanReportSubDev
  toEnum 244 = LanScene
  toEnum 245 = LanPublishCloudConfig
  toEnum 246 = LanPublicAppConfig
  toEnum 247 = LanExportAppConfig
  toEnum 248 = LanPublishScenePanel
  toEnum 249 = LanRemoveGw
  toEnum 250 = LanCheckGwUpdate
  toEnum 251 = LanGwUpdate
  toEnum 252 = LanSetGwChannel