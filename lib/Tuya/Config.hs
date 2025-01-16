{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tuya.Config where

import Data.Aeson (FromJSON (..), Options (..), genericParseJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Aeson.Types (withText)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import qualified Network.MQTT.Client as MQTT
import Network.URI as Network

data Config = Config
  { cfgMqtt :: MqttSettings
  , cfgTuya :: TuyaSettings
  }
  deriving (Eq, Show, Generic)

data MqttSettings = MqttSettings
  { mqttBrokerUri :: Network.URI
  , mqttProtocol :: MQTT.ProtocolLevel
  }
  deriving (Eq, Show, Generic)

data TuyaSettings = TuyaSettings
  { tuyaClientId :: Text
  , tuyaClientSecret :: Text
  , tuyaAppUserId :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON opts

instance FromJSON MqttSettings where
  parseJSON = genericParseJSON opts

instance FromJSON TuyaSettings where
  parseJSON = genericParseJSON opts

{- Orphans -}

instance FromJSON MQTT.ProtocolLevel where
  parseJSON =
    withText "Protocol" $ \case
      "3.1.1" -> pure MQTT.Protocol311
      "5.0" -> pure MQTT.Protocol50
      _ -> fail "Unsupported protocol"

opts :: Options
opts = (aesonPrefix snakeCase){omitNothingFields = True}

readConfigFile :: FilePath -> IO Config
readConfigFile = Yaml.decodeFileThrow
