{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tuya.Poll where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.PQueue.Prio.Min as PQ
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import Data.Time.Clock
import Data.Time.Format
import qualified Network.MQTT.Client as MQTT
import qualified Network.MQTT.Topic as MQTT
import qualified Network.Socket as S

import Tuya.Config
import Tuya.Local
import Tuya.Types

data Env = Env
  { envCfg :: Config
  , envMc :: MQTT.MQTTClient
  , envPollers :: IORef (HashMap Text (Async ()))
  , envIps :: IORef (HashMap Text Text)
  , envKeys :: IORef (HashMap Text BS.ByteString)
  , envSpecs :: IORef (HashMap Text Specification)
  , envStatusMap :: IORef (HashMap Text (HashMap Text Text))
  , envSchedule :: IORef (PQ.MinPQueue UTCTime Text)
  }

poller :: Config -> IO ()
poller cfg = do
  mc <- MQTT.connectURI MQTT.mqttConfig{MQTT._protocol = mqttProtocol (cfgMqtt cfg)} (mqttBrokerUri (cfgMqtt cfg))
  env <- Env cfg mc <$> newIORef mempty <*> newIORef mempty <*> newIORef mempty <*> newIORef mempty <*> newIORef mempty <*> newIORef mempty
  sub env

sub :: Env -> IO ()
sub env = do
  let cfg = envCfg env
  mc <-
    MQTT.connectURI
      MQTT.mqttConfig{MQTT._protocol = mqttProtocol (cfgMqtt cfg), MQTT._msgCB = MQTT.SimpleCallback (msgReceived env)}
      (mqttBrokerUri (cfgMqtt cfg))
  _ <-
    MQTT.subscribe
      mc
      [ ("tuya/device/+/ip", MQTT.subOptions)
      , ("tuya/device/+/key", MQTT.subOptions)
      , ("tuya/device/+/spec", MQTT.subOptions)
      ]
      []
  MQTT.waitForClient mc

msgReceived :: Env -> MQTT.MQTTClient -> MQTT.Topic -> LBS.ByteString -> [MQTT.Property] -> IO ()
msgReceived env _mc topic payload _
  | MQTT.match "tuya/device/+/ip" topic = do
      let devId = MQTT.unTopic $ MQTT.split topic !! 2
      cancelPoller env devId
      update
        devId
        (decodeUtf8 $ LBS.toStrict payload)
        (envIps env)
      startPoller env devId
  | MQTT.match "tuya/device/+/key" topic =
      update
        (MQTT.unTopic $ MQTT.split topic !! 2)
        (LBS.toStrict payload)
        (envKeys env)
  | MQTT.match "tuya/device/+/spec" topic = do
      let spec = either error dsSpecification $ eitherDecode payload
      update
        (MQTT.unTopic $ MQTT.split topic !! 2)
        spec
        (envSpecs env)
      update
        (MQTT.unTopic $ MQTT.split topic !! 2)
        (statusMap spec)
        (envStatusMap env)
  | otherwise = return ()

update :: Text -> v -> IORef (HashMap Text v) -> IO ()
update devId val ref = modifyIORef' ref (HM.insert devId val)

statusMap :: Specification -> HashMap Text Text
statusMap spec = mconcat $ map go (specStatus spec)
 where
  go s = HM.singleton (Text.pack $ show $ statusDpId s) (statusCode s)

startPoller :: Env -> Text -> IO ()
startPoller env devId = do
  a <- async (devicePoller env devId)
  modifyIORef' (envPollers env) $ HM.insert devId a

cancelPoller :: Env -> Text -> IO ()
cancelPoller env devId = do
  a <- atomicModifyIORef' (envPollers env) $ \pollers ->
    case HM.lookup devId pollers of
      Just a -> (HM.delete devId pollers, Just a)
      Nothing -> (pollers, Nothing)
  traverse_ cancel a

devicePoller :: Env -> Text -> IO ()
devicePoller env devId = forever $ pollDevice env devId

pollDevice :: Env -> Text -> IO ()
pollDevice env devId = do
  ips <- readIORef (envIps env)
  keys <- readIORef (envKeys env)
  maps <- readIORef (envStatusMap env)
  case (HM.lookup devId ips, HM.lookup devId keys, HM.lookup devId maps) of
    (Just ip, Just key, Just smap) -> do
      sockaddr <- sockAddrForIp ip
      bracket (connect sockaddr Tuya33 key) close $ \c -> do
        t <- getT'
        sendCmd c DpQuery (GetDeviceStatus devId devId t devId)
        v <- recvMsg c
        case v of
          Left err -> error $ "recvMesg failed: " <> err
          Right msg -> deviceStatus env devId smap (msgPayload msg)
      threadDelay 10000000
    _ -> threadDelay 10000000
 where
  getT' = Text.pack . formatTime defaultTimeLocale "%s" <$> getCurrentTime

data GetDeviceStatus = GetDeviceStatus
  { gdsGwId :: Text
  , gdsDevId :: Text
  , gdsT :: Text
  , gdsUid :: Text
  }
  deriving (Show)

newtype DeviceStatus = DeviceStatus {sDps :: Object}
  deriving (Show)

instance ToJSON GetDeviceStatus where
  toJSON GetDeviceStatus{..} =
    object
      [ "gwId" .= gdsGwId
      , "devId" .= gdsDevId
      , "t" .= gdsT
      , "dps" .= toJSON (mempty :: Object)
      , "uid" .= gdsUid
      ]

instance FromJSON DeviceStatus where
  parseJSON =
    withObject "DeviceStatus" $ \o ->
      DeviceStatus <$> o .: "dps"

deviceStatus :: Env -> Text -> HashMap Text Text -> DeviceStatus -> IO ()
deviceStatus env devId smap status = do
  forM_ (KeyMap.toList (sDps status)) $ \(dp, val) -> do
    case HM.lookup (Key.toText dp) smap of
      Just code -> do
        let Just topic = MQTT.mkTopic ("tuya/device/" <> devId <> "/status/" <> code)
        MQTT.publish (envMc env) topic (encode val) True
      Nothing -> return ()

sockAddrForIp :: Text -> IO S.SockAddr
sockAddrForIp ip = do
  let hints = S.defaultHints{S.addrFlags = [S.AI_NUMERICHOST], S.addrSocketType = S.Stream}
  addr : _ <- S.getAddrInfo (Just hints) (Just $ Text.unpack ip) (Just "6668")
  return (S.addrAddress addr)
