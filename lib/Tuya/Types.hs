{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Tuya.Types where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Network.Socket (Socket)
import NoThunks.Class

import Control.DeepSeq
import Tuya.Orphans ()

data Raw = Raw
  { rawPrefix :: !Word32
  , rawSequence :: !Word32
  , rawCommand :: !Word32
  , rawPayloadSize :: !Word32
  , rawReturnCode :: !Word32
  , rawPayload :: !ByteString
  , rawCrc :: !Word32
  , rawSuffix :: !Word32
  }
  deriving (Show, Generic, NoThunks, NFData)

data Msg a = Msg
  { msgSequence :: !Word32
  , msgCommand :: !CommandType
  , msgReturnCode :: !Word32
  , msgPayload :: !a
  }
  deriving (Show, Generic, NoThunks, NFData)

data Gateway = Gateway
  { gwIp :: !Text
  , gwGwId :: !Text
  , gwActive :: !Int
  , gwEncrypt :: !Bool
  , gwProductKey :: !Text
  , gwVersion :: !Text
  }
  deriving (Show, Generic, NoThunks, NFData)

data Protocol = Tuya33 | Tuya34
  deriving (Show, Eq, Ord, Generic, NoThunks, NFData)

data Client = Client
  { clientSocket :: !Socket
  , clientProtocol :: !Protocol
  , clientLocalKey :: !ByteString
  , clientSequenceNumber :: !(IORef Word32)
  }
  deriving stock (Generic)
  deriving (NoThunks) via AllowThunksIn '["clientSocket"] Client

data Device = Device
  { deviceId :: !Text
  , deviceName :: !Text
  , deviceCategory :: !Text
  , deviceCategoryName :: !Text
  , deviceLocalKey :: !Text
  , deviceModel :: !Text
  , deviceProductId :: !Text
  , deviceProductName :: !Text
  , deviceOnline :: !Bool
  , deviceUuid :: !Text
  }
  deriving (Show, Generic, NoThunks, NFData)

data Specification = Specification
  { specCategory :: !Text
  , specFunctions :: !(Vector Function)
  , specStatus :: !(Vector Status)
  }
  deriving (Show, Generic, NoThunks, NFData)

data DeviceSpecification = DeviceSpecification
  { dsDevice :: !Device
  , dsSpecification :: !Specification
  }
  deriving (Show, Generic, NoThunks, NFData)

data Function = Function
  { funcCode :: !Text
  , funcDpId :: !Integer
  , funcName :: !Text
  , funcType :: !Text
  , funcValues :: !Values
  }
  deriving (Show, Generic, NoThunks, NFData)

data Status = Status
  { statusCode :: !Text
  , statusDpId :: !Integer
  , statusName :: !Text
  , statusType :: !Text
  , statusValues :: !Values
  }
  deriving (Show, Generic, NoThunks, NFData)

data Values = Values
  { valUnit :: !(Maybe Text)
  , valMin :: !(Maybe Integer)
  , valMax :: !(Maybe Integer)
  , valScale :: !(Maybe Integer)
  , valStep :: !(Maybe Integer)
  , valRange :: !(Maybe (Vector Text))
  , valLabel :: !(Maybe (Vector Text))
  , valMaxLen :: !(Maybe Integer)
  }
  deriving (Show, Generic, NoThunks, NFData)

{- FOURMOLU_DISABLE -}
instance FromJSON Gateway where
  parseJSON :: Value -> Parser Gateway
  parseJSON = withObject "Gateway" $ \v ->
    Gateway
      <$> v .: "ip"
      <*> v .: "gwId"
      <*> v .: "active"
      <*> v .: "encrypt"
      <*> v .: "productKey"
      <*> v .: "version"

instance FromJSON Device where
  parseJSON = withObject "Device" $ \v ->
    Device
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "category"
      <*> v .: "category_name"
      <*> v .: "local_key"
      <*> v .: "model"
      <*> v .: "product_id"
      <*> v .: "product_name"
      <*> v .: "online"
      <*> v .: "uuid"

instance ToJSON Device where
  toJSON Device{..} =
    object
      [
      "id" .= deviceId
      , "name" .= deviceName
      , "category" .= deviceCategory
      , "category_name" .= deviceCategoryName
      , "local_key" .= deviceLocalKey
      , "model" .= deviceModel
      , "product_id" .= deviceProductId
      , "product_name" .= deviceProductName
      , "online" .= deviceOnline
      , "uuid" .= deviceUuid
      ]

instance FromJSON Specification where
  parseJSON =
    withObject "Specification" $ \v ->
      Specification
        <$> v .: "category"
        <*> v .: "functions"
        <*> v .: "status"

instance ToJSON Specification where
  toJSON Specification{..} =
    object
      [ "category" .= specCategory
      , "functions" .= specFunctions
      , "status" .= specStatus
      ]

instance FromJSON Function where
  parseJSON =
    withObject "Function" $ \v ->
      Function
        <$> v .: "code"
        <*> v .:? "dp_id" .!= 0
        <*> v .:? "name" .!= ""
        <*> v .: "type"
        <*> v .: "values"

instance ToJSON Function where
  toJSON Function{..} =
    object
      [ "code" .= funcCode
      , "dp_id" .= funcDpId
      , "name" .= funcName
      , "type" .= funcType
      , "values" .= funcValues
      ]

instance FromJSON Status where
  parseJSON =
    withObject "Status" $ \v ->
      Status
        <$> v .: "code"
        <*> v .:? "dp_id" .!= 0
        <*> v .:? "name" .!= ""
        <*> v .: "type"
        <*> v .: "values"

instance ToJSON Status where
  toJSON Status{..} =
    object
      [ "code" .= statusCode
      , "dp_id" .= statusDpId
      , "name" .= statusName
      , "type" .= statusType
      , "values" .= statusValues
      ]

instance FromJSON Values where
  parseJSON a = go a <|> withEmbeddedJSON "EmbeddedValue" go a
   where
    go =
      withObject "Values" $ \v ->
        Values
          <$> v .:? "unit"
          <*> v .:? "min"
          <*> v .:? "max"
          <*> v .:? "scale"
          <*> v .:? "step"
          <*> v .:? "range"
          <*> v .:? "label"
          <*> v .:? "maxlen"

instance ToJSON Values where
  toJSON Values{..} =
    noNulls
      [ "unit" .= valUnit
      , "min" .= valMin
      , "max" .= valMax
      , "scale" .= valScale
      , "range" .= valRange
      , "label" .= valLabel
      , "maxlen" .= valMaxLen
      ]

instance FromJSON DeviceSpecification where
  parseJSON =
    withObject "DeviceSpecification" $ \v ->
      DeviceSpecification
        <$> v .: "dev"
        <*> v .: "spec"

instance ToJSON DeviceSpecification where
  toJSON DeviceSpecification{..} =
    object
      [ "dev" .= dsDevice
      , "spec" .= dsSpecification
      ]
{- FOURMOLU_ENABLE -}

data CommandType
  = Udp
  | ApConfig
  | Active
  | SessKeyNegStart
  | SessKeyNegRes
  | SessKeyNegFinish
  | Unbind
  | Control
  | Status'
  | HeartBeat
  | DpQuery
  | QueryWifi
  | TokenBind
  | ControlNew
  | EnableWifi
  | DpQueryNew
  | SceneExecute
  | DpRefresh
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
  deriving (Show, Eq, Ord, Generic, NoThunks, NFData)

-- | Wire value of a command. Values follow tinytuya's command_types.py.
commandToWord :: CommandType -> Word32
commandToWord Udp = 0
commandToWord ApConfig = 1
commandToWord Active = 2
commandToWord SessKeyNegStart = 3
commandToWord SessKeyNegRes = 4
commandToWord SessKeyNegFinish = 5
commandToWord Unbind = 6
commandToWord Control = 7
commandToWord Status' = 8
commandToWord HeartBeat = 9
commandToWord DpQuery = 10
commandToWord QueryWifi = 11
commandToWord TokenBind = 12
commandToWord ControlNew = 13
commandToWord EnableWifi = 14
commandToWord DpQueryNew = 16
commandToWord SceneExecute = 17
commandToWord DpRefresh = 18
commandToWord UdpNew = 19
commandToWord ApConfigNew = 20
commandToWord BroadcastLpv34 = 35
commandToWord LanExtStream = 64
commandToWord LanGwActive = 240
commandToWord LanSubDevRequest = 241
commandToWord LanDeleteSubDev = 242
commandToWord LanReportSubDev = 243
commandToWord LanScene = 244
commandToWord LanPublishCloudConfig = 245
commandToWord LanPublicAppConfig = 246
commandToWord LanExportAppConfig = 247
commandToWord LanPublishScenePanel = 248
commandToWord LanRemoveGw = 249
commandToWord LanCheckGwUpdate = 250
commandToWord LanGwUpdate = 251
commandToWord LanSetGwChannel = 252

-- | Decode a wire value, returning 'Nothing' for commands we don't know.
commandFromWord :: Word32 -> Maybe CommandType
commandFromWord 0 = Just Udp
commandFromWord 1 = Just ApConfig
commandFromWord 2 = Just Active
commandFromWord 3 = Just SessKeyNegStart
commandFromWord 4 = Just SessKeyNegRes
commandFromWord 5 = Just SessKeyNegFinish
commandFromWord 6 = Just Unbind
commandFromWord 7 = Just Control
commandFromWord 8 = Just Status'
commandFromWord 9 = Just HeartBeat
commandFromWord 10 = Just DpQuery
commandFromWord 11 = Just QueryWifi
commandFromWord 12 = Just TokenBind
commandFromWord 13 = Just ControlNew
commandFromWord 14 = Just EnableWifi
commandFromWord 16 = Just DpQueryNew
commandFromWord 17 = Just SceneExecute
commandFromWord 18 = Just DpRefresh
commandFromWord 19 = Just UdpNew
commandFromWord 20 = Just ApConfigNew
commandFromWord 35 = Just BroadcastLpv34
commandFromWord 64 = Just LanExtStream
commandFromWord 240 = Just LanGwActive
commandFromWord 241 = Just LanSubDevRequest
commandFromWord 242 = Just LanDeleteSubDev
commandFromWord 243 = Just LanReportSubDev
commandFromWord 244 = Just LanScene
commandFromWord 245 = Just LanPublishCloudConfig
commandFromWord 246 = Just LanPublicAppConfig
commandFromWord 247 = Just LanExportAppConfig
commandFromWord 248 = Just LanPublishScenePanel
commandFromWord 249 = Just LanRemoveGw
commandFromWord 250 = Just LanCheckGwUpdate
commandFromWord 251 = Just LanGwUpdate
commandFromWord 252 = Just LanSetGwChannel
commandFromWord _ = Nothing

noNulls :: [(Key, Value)] -> Value
noNulls = object . filter ((/= Null) . snd)
