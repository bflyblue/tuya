{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Tuya.Devices where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Text (Text)
import Data.Text.Encoding
import GHC.Generics (Generic)
import qualified Network.MQTT.Client as MQTT
import qualified Network.MQTT.Topic as MQTT
import NoThunks.Class

import Tuya.Cloud
import Tuya.Config
import Tuya.Orphans ()
import Tuya.Types

data Env = Env
  { envCfg :: Config
  , envIps :: IORef (HashMap Text Text)
  , envKeys :: IORef (HashMap Text Text)
  , envVers :: IORef (HashMap Text Text)
  }
  deriving stock (Generic)
  deriving (NoThunks) via AllowThunksIn '["envCfg"] Env

serve :: Config -> IO ()
serve cfg = do
  env <- Env cfg <$> newIORef mempty <*> newIORef mempty <*> newIORef mempty
  mc <-
    MQTT.connectURI
      MQTT.mqttConfig{MQTT._protocol = mqttProtocol (cfgMqtt cfg), MQTT._msgCB = MQTT.SimpleCallback (msgReceived env)}
      (mqttBrokerUri (cfgMqtt cfg))
  _ <- MQTT.subscribe mc [("tuya/device/+/discover", MQTT.subOptions), ("tuya/device/+/ip", MQTT.subOptions)] []
  logger env mc
  MQTT.waitForClient mc

logger :: Env -> MQTT.MQTTClient -> IO ()
logger env mc = go
 where
  go = do
    threadDelay 60000000
    ips <- readIORef (envIps env)
    keys <- readIORef (envKeys env)
    vers <- readIORef (envVers env)
    putStrLn $
      "devices: "
        ++ show (HM.size ips)
        ++ " ips, "
        ++ show (HM.size keys)
        ++ " keys, "
        ++ show (HM.size vers)
        ++ " vers."
    connected <- MQTT.isConnected mc
    nt <- noThunks [] env
    case nt of
      Just ti -> putStrLn $ "device no thunks: " ++ show ti
      Nothing -> return ()
    when connected go

msgReceived :: Env -> MQTT.MQTTClient -> MQTT.Topic -> ByteString -> [MQTT.Property] -> IO ()
msgReceived env mc topic payload _
  | MQTT.match "tuya/device/+/discover" topic = discoverDevice env mc (MQTT.unTopic $ MQTT.split topic !! 2) payload
  | MQTT.match "tuya/device/+/ip" topic = getDeviceDetails env mc (MQTT.unTopic $ MQTT.split topic !! 2)
  | otherwise = return ()

discoverDevice :: Env -> MQTT.MQTTClient -> Text -> ByteString -> IO ()
discoverDevice env mc devId payload = do
  case decode' payload of
    Just gw -> do
      ips <- readIORef (envIps env)
      case HM.lookup devId ips of
        Just ip
          | ip == gwIp gw -> return ()
          | otherwise -> newIp devId (gwIp gw)
        Nothing -> newIp devId (gwIp gw)
      vers <- readIORef (envVers env)
      case HM.lookup devId vers of
        Just ver
          | ver == gwVersion gw -> return ()
          | otherwise -> newVersion devId (gwVersion gw)
        Nothing -> newVersion devId (gwVersion gw)
    Nothing -> return ()
 where
  newIp devid ip = do
    let Just topic = MQTT.mkTopic ("tuya/device/" <> devid <> "/ip")
    MQTT.publish mc topic (fromStrict $ encodeUtf8 ip) True
    modifyIORef' (envIps env) (HM.insert devId ip)
  newVersion devid ver = do
    let Just topic = MQTT.mkTopic ("tuya/device/" <> devid <> "/version")
    MQTT.publish mc topic (fromStrict $ encodeUtf8 ver) True
    modifyIORef' (envVers env) (HM.insert devId ver)

getDeviceDetails :: Env -> MQTT.MQTTClient -> Text -> IO ()
getDeviceDetails env mc devId = do
  let tuya = cfgTuya (envCfg env)
      cloudAuth = CloudAuth (encodeUtf8 $ tuyaClientId tuya) (encodeUtf8 $ tuyaClientSecret tuya) (tuyaAppUserId tuya)
  runCloud cloudAuth $ do
    devices <- getDevices [devId]
    forM_ devices $ \dev -> do
      keys <- liftIO $ readIORef (envKeys env)
      case HM.lookup (deviceId dev) keys of
        Just key
          | key == deviceLocalKey dev -> newKey (deviceId dev) dev (deviceLocalKey dev)
          | otherwise -> return ()
        Nothing -> newKey (deviceId dev) dev (deviceLocalKey dev)
 where
  newKey devid device key = do
    liftIO $ modifyIORef' (envKeys env) (HM.insert devid key)
    let Just keyTopic = MQTT.mkTopic ("tuya/device/" <> devid <> "/key")
    liftIO $ MQTT.publish mc keyTopic (fromStrict $ encodeUtf8 key) True
    spec <- getDeviceSpecification devid
    let Just specTopic = MQTT.mkTopic ("tuya/device/" <> devid <> "/spec")
        devspec = DeviceSpecification device spec
    liftIO $ MQTT.publish mc specTopic (encode devspec) True
