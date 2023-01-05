{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tuya.Types where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Word (Word32)
import Network.Socket (SockAddr, Socket)

data Raw = Raw
  { rawPrefix :: Word32
  , rawSequence :: Word32
  , rawCommand :: Word32
  , rawPayloadSize :: Word32
  , rawReturnCode :: Word32
  , rawPayload :: ByteString
  , rawCrc :: Word32
  , rawSuffix :: Word32
  }
  deriving (Show)

data Msg a = Msg
  { msgSequence :: Word32
  , msgCommand :: CommandType
  , msgReturnCode :: Word32
  , msgPayload :: a
  }
  deriving (Show, Functor)

data Gateway = Gateway
  { gwIp :: Text
  , gwGwId :: Text
  , gwActive :: Int
  , gwEncrypt :: Bool
  , gwProductKey :: Text
  , gwVersion :: Text
  }
  deriving (Show)

data Protocol = Tuya33
  deriving (Show)

data Client = Client
  { clientSocket :: Socket
  , clientProtocol :: Protocol
  , clientLocalKey :: ByteString
  , clientSequenceNumber :: IORef Word32
  }

data Device = Device
  { deviceId :: Text
  , deviceName :: Text
  , deviceCategory :: Text
  , deviceCategoryName :: Text
  , deviceLocalKey :: Text
  , deviceModel :: Text
  , deviceProductId :: Text
  , deviceProductName :: Text
  , deviceOnline :: Bool
  , deviceUuid :: Text
  }
  deriving (Show)

data Specification = Specification
  { specCategory :: Text
  , specFunctions :: [Function]
  , specStatus :: [Status]
  }
  deriving (Show)

data DeviceSpecification = DeviceSpecification
  { dsDevice :: Device
  , dsSpecification :: Specification
  }
  deriving (Show)

data Function = Function
  { funcCode :: Text
  , funcDpId :: Integer
  , funcName :: Text
  , funcType :: Text
  , funcValues :: Values
  }
  deriving (Show)

data Status = Status
  { statusCode :: Text
  , statusDpId :: Integer
  , statusName :: Text
  , statusType :: Text
  , statusValues :: Values
  }
  deriving (Show)

data Values = Values
  { valUnit :: Maybe Text
  , valMin :: Maybe Integer
  , valMax :: Maybe Integer
  , valScale :: Maybe Integer
  , valStep :: Maybe Integer
  , valRange :: Maybe [Text]
  , valLabel :: Maybe [Text]
  , valMaxLen :: Maybe Integer
  }
  deriving (Show)

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

data Discovered = Discovered
  { discoveredIds :: HashMap Text Gateway
  , discoveredAddrs :: Map SockAddr Gateway
  }
  deriving (Show)

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
  fromEnum Status' = 8
  fromEnum HeartBeat = 9
  fromEnum DpQuery = 10
  fromEnum QueryWifi = 100
  fromEnum TokenBind = 12
  fromEnum ControlNew = 13
  fromEnum EnableWifi = 14
  fromEnum DpQueryNew = 16
  fromEnum SceneExecute = 17
  fromEnum DpRefresh = 18
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
  toEnum 8 = Status'
  toEnum 9 = HeartBeat
  toEnum 10 = DpQuery
  toEnum 100 = QueryWifi
  toEnum 12 = TokenBind
  toEnum 13 = ControlNew
  toEnum 14 = EnableWifi
  toEnum 16 = DpQueryNew
  toEnum 17 = SceneExecute
  toEnum 18 = DpRefresh
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

noNulls :: [(Key, Value)] -> Value
noNulls = object . filter ((/= Null) . snd)
