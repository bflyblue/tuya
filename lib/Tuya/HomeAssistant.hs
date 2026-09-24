{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Tuya.HomeAssistant where

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Scientific (base10Exponent, coefficient, scientific)
import Data.Text hiding (show)
import GHC.Generics (Generic)
import qualified Network.MQTT.Client as MQTT
import qualified Network.MQTT.Topic as MQTT
import NoThunks.Class

import Control.DeepSeq
import qualified Data.ByteString.Lazy as LBS
import Tuya.Config
import Tuya.Mqtt
import Tuya.Orphans ()
import Tuya.Types

newtype Env = Env
  { envSpecs :: IORef (HashMap (Text, Text) Status)
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

homeAssist :: Config -> IO ()
homeAssist cfg = do
  env <- Env <$> newIORef mempty
  mc <-
    MQTT.connectURI
      MQTT.mqttConfig{MQTT._protocol = mqttProtocol (cfgMqtt cfg), MQTT._msgCB = MQTT.SimpleCallback (msgReceived env)}
      (mqttBrokerUri (cfgMqtt cfg))
  _ <-
    MQTT.subscribe
      mc
      [ ("tuya/device/+/spec", MQTT.subOptions)
      , ("tuya/device/+/status/+", MQTT.subOptions)
      ]
      []
  logger env mc
  MQTT.waitForClient mc

logger :: Env -> MQTT.MQTTClient -> IO ()
logger env mc = go
 where
  go = do
    threadDelay 60000000
    specs <- readIORef (envSpecs env)
    putStrLn $
      "homeassist: "
        ++ show (HM.size specs)
        ++ " specs."
    connected <- MQTT.isConnected mc
    nt <- noThunks [] env
    case nt of
      Just ti -> putStrLn $ "homeassist no thunks: " ++ show ti
      Nothing -> return ()
    when connected go

msgReceived :: Env -> MQTT.MQTTClient -> MQTT.Topic -> ByteString -> [MQTT.Property] -> IO ()
msgReceived env mc topic payload _ =
  case MQTT.unTopic <$> MQTT.split topic of
    ["tuya", "device", devId, "spec"] -> specification env mc devId payload
    ["tuya", "device", devId, "status", code] -> status env mc devId code payload
    _ -> return ()

eitherDecodeDeep :: (FromJSON b, NFData b) => LBS.ByteString -> Either String b
eitherDecodeDeep str =
  case eitherDecode' str of
    Left err -> Left err
    Right parsed -> parsed `deepseq` Right parsed

specification :: Env -> MQTT.MQTTClient -> Text -> ByteString -> IO ()
specification env mc devId payload =
  case eitherDecodeDeep payload of
    Left err -> putStrLn $ "homeassist: ignoring spec for " <> show devId <> ": " <> err
    Right devspec -> do
      let
        dev = dsDevice devspec
        spec = dsSpecification devspec

      forM_ (specStatus spec) $ \st -> do
        modifyIORef' (envSpecs env) (HM.insert (devId, statusCode st) st)
        let config (comp, cfg) =
              publishLevels mc ["homeassistant", comp, devId <> "_" <> statusCode st, "config"] (encode cfg) True

        case statusType st of
          "Boolean" -> config (boolean devId dev st)
          "String" -> config (sensor devId dev st)
          "Enum" -> config (sensor devId dev st)
          "Integer" -> config (sensor devId dev st)
          _ -> return ()

sensor :: Text -> Device -> Status -> (Text, Value)
sensor devId dev st =
  let
    stopic = "tuya/device/" <> devId <> "/value/" <> statusCode st
    config =
      noNulls
        [ "name" .= statusName st
        , "device"
            .= object
              [ "identifiers" .= deviceUuid dev
              , "model" .= deviceProductName dev
              , "name" .= deviceName dev
              ]
        , "state_topic" .= stopic
        , "unique_id" .= (deviceUuid dev <> "-" <> statusCode st)
        , "unit_of_measurement" .= valUnit (statusValues st)
        , "value_template" .= ("{{ value_json }}" :: Text)
        ]
   in
    ("sensor", config)

boolean :: Text -> Device -> Status -> (Text, Value)
boolean devId dev st =
  let
    stopic = "tuya/device/" <> devId <> "/value/" <> statusCode st
    config =
      noNulls
        [ "name" .= statusName st
        , "device"
            .= object
              [ "identifiers" .= deviceUuid dev
              , "model" .= deviceProductName dev
              , "name" .= deviceName dev
              ]
        , "state_topic" .= stopic
        , "payload_on" .= ("On" :: Text)
        , "payload_off" .= ("Off" :: Text)
        , "unique_id" .= (deviceUuid dev <> "-" <> statusCode st)
        , "value_template" .= ("{{ value_json }}" :: Text)
        ]
   in
    ("binary_sensor", config)

status :: Env -> MQTT.MQTTClient -> Text -> Text -> ByteString -> IO ()
status env mc devId code payload = do
  specs <- readIORef (envSpecs env)
  case HM.lookup (devId, code) specs of
    Just st -> do
      let val =
            case decode payload of
              Just v ->
                encode $
                  case statusType st of
                    "Boolean" ->
                      case v of
                        Bool b -> if b then String "On" else String "Off"
                        _ -> v
                    "Integer" -> scaled (valScale (statusValues st)) v
                    _ -> v
              Nothing -> payload
      publishDevice mc devId ["value", code] val True
    Nothing ->
      return ()
 where
  -- Shift the decimal exponent directly: exact, and total for negative scales.
  scaled (Just s) (Number n) = Number $ scientific (coefficient n) (base10Exponent n - fromInteger s)
  scaled _ v = v

{-
formatValue :: Text -> Values -> Value -> LBS.ByteString
formatValue ty values val =
  encode $
    noNulls $
      ["raw" .= val, "type" .= ty, "unit" .= valUnit values]
        ++ case ty of
          "Boolean" -> ["value" .= val]
          "String" -> ["value" .= val, "maxlen" .= valMaxLen values]
          "Enum" -> ["value" .= val, "range" .= valRange values]
          "Integer" -> ["value" .= scaled (valScale values) val, "min" .= valMin values, "max" .= valMax values, "scale" .= valScale values, "step" .= valStep values]
          "Bitmap" -> ["value" .= bitmap (valLabel values) val, "label" .= valLabel values, "maxlen" .= valMaxLen values]
          _ -> ["value" .= val]
 where
  scaled (Just s) (Number n) = Number $ n / (10 ^ s)
  scaled _ v = v

  bitmap (Just labels) (Number n) = case (floatingOrInteger n :: Either Double Int) of
    Left _ -> Number n
    Right bits -> object $ zipWith (\b label -> Key.fromText label .= testBit bits b) [0 ..] labels
  bitmap _ v = v
-}
