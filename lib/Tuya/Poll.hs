{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Tuya.Poll where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (cipherInit, ecbEncrypt)
import Crypto.Error (throwCryptoError)
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC (hmacGetDigest), hmac)
import Crypto.Random (MonadRandom (getRandomBytes))
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteArray (convert, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import qualified Data.Text.IO as Text
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Vector as Vec
import GHC.Generics (Generic)
import qualified Network.MQTT.Client as MQTT
import qualified Network.MQTT.Topic as MQTT
import qualified Network.Socket as S
import NoThunks.Class
import System.Timeout

import Control.DeepSeq
import Tuya.Config
import Tuya.Local
import Tuya.Orphans ()
import Tuya.Types

data Env = Env
  { envCfg :: Config
  , envMc :: MQTT.MQTTClient
  , envPollers :: IORef (HashMap Text (Async ()))
  , envIps :: IORef (HashMap Text Text)
  , envVers :: IORef (HashMap Text (Maybe Protocol))
  , envKeys :: IORef (HashMap Text BS.ByteString)
  , envSpecs :: IORef (HashMap Text Specification)
  , envStatusMap :: IORef (HashMap Text (HashMap Text Text))
  }
  deriving stock (Generic)
  deriving (NoThunks) via AllowThunksIn '["envCfg", "envMc", "envPollers"] Env

poller :: Config -> IO ()
poller cfg = do
  mc <- MQTT.connectURI MQTT.mqttConfig{MQTT._protocol = mqttProtocol (cfgMqtt cfg)} (mqttBrokerUri (cfgMqtt cfg))
  env <- Env cfg mc <$> newIORef mempty <*> newIORef mempty <*> newIORef mempty <*> newIORef mempty <*> newIORef mempty <*> newIORef mempty
  reaper env `concurrently_` sub env

reaper :: Env -> IO ()
reaper env = forever $ do
  pollers <- HM.toList <$> readIORef (envPollers env)
  case pollers of
    [] -> threadDelay 100000
    _nonEmpty -> do
      mc <- timeout 1000000 $ waitAnyCatch (snd <$> pollers)
      case mc of
        Just (a, r) -> do
          let devIds = map fst $ List.filter ((== a) . snd) pollers
          case r of
            Left e -> putStrLn $ "Poller for " <> show devIds <> " threw exception " <> show e
            Right v -> putStrLn $ "Poller for " <> show devIds <> " exited with result" <> show v
          forM_ devIds $ \d -> modifyIORef' (envPollers env) (HM.delete d)
        Nothing ->
          threadDelay 100000

sub :: Env -> IO ()
sub env = do
  r <- try go
  case r of
    Left e -> do
      putStrLn $ "MQTT exception: " <> show (e :: MQTT.MQTTException)
      threadDelay 1000000
      sub env
    Right _ -> return ()
 where
  go = do
    let cfg = envCfg env
    mc <-
      MQTT.connectURI
        MQTT.mqttConfig{MQTT._protocol = mqttProtocol (cfgMqtt cfg), MQTT._msgCB = MQTT.SimpleCallback (msgReceived env)}
        (mqttBrokerUri (cfgMqtt cfg))
    _ <-
      MQTT.subscribe
        mc
        [ ("tuya/device/+/discover", MQTT.subOptions)
        , ("tuya/device/+/ip", MQTT.subOptions)
        , ("tuya/device/+/version", MQTT.subOptions)
        , ("tuya/device/+/key", MQTT.subOptions)
        , ("tuya/device/+/spec", MQTT.subOptions)
        ]
        []
    MQTT.waitForClient mc

logger :: Env -> MQTT.MQTTClient -> IO ()
logger env mc = go
 where
  go = do
    threadDelay 60000000
    pollers <- readIORef (envPollers env)
    ips <- readIORef (envIps env)
    keys <- readIORef (envKeys env)
    vers <- readIORef (envVers env)
    specs <- readIORef (envSpecs env)
    statusmap <- readIORef (envStatusMap env)
    putStrLn $
      "poll: "
        ++ show (HM.size pollers)
        ++ " pollers, "
        ++ show (HM.size ips)
        ++ " ips, "
        ++ show (HM.size keys)
        ++ " keys, "
        ++ show (HM.size vers)
        ++ " vers, "
        ++ show (HM.size specs)
        ++ " specs, "
        ++ show (HM.size statusmap)
        ++ " statusmap."
    connected <- MQTT.isConnected mc
    nt <- noThunks [] env
    case nt of
      Just ti -> putStrLn $ "poll no thunks: " ++ show ti
      Nothing -> return ()
    when connected go

msgReceived :: Env -> MQTT.MQTTClient -> MQTT.Topic -> LBS.ByteString -> [MQTT.Property] -> IO ()
msgReceived env _mc topic payload _
  | MQTT.match "tuya/device/+/discover" topic = do
      let devId = MQTT.unTopic $ MQTT.split topic !! 2
      ensurePoller env devId
  | MQTT.match "tuya/device/+/ip" topic = do
      let devId = MQTT.unTopic $ MQTT.split topic !! 2
      update
        devId
        (decodeUtf8 $ LBS.toStrict payload)
        (envIps env)
      cancelPoller env devId
  | MQTT.match "tuya/device/+/version" topic = do
      let devId = MQTT.unTopic $ MQTT.split topic !! 2
      let mproto = case decodeUtf8 (LBS.toStrict payload) of
            "3.3" -> Just Tuya33
            "3.4" -> Just Tuya34
            _ -> Nothing
      update devId mproto (envVers env)
      cancelPoller env devId
  | MQTT.match "tuya/device/+/key" topic =
      update
        (MQTT.unTopic $ MQTT.split topic !! 2)
        (LBS.toStrict payload)
        (envKeys env)
  | MQTT.match "tuya/device/+/spec" topic = do
      let spec = either error dsSpecification $ eitherDecodeDeep payload
      update
        (MQTT.unTopic $ MQTT.split topic !! 2)
        spec
        (envSpecs env)
      update
        (MQTT.unTopic $ MQTT.split topic !! 2)
        (statusMap spec)
        (envStatusMap env)
  | otherwise = return ()

eitherDecodeDeep :: (FromJSON b, NFData b) => LBS.ByteString -> Either String b
eitherDecodeDeep str =
  case eitherDecode' str of
    Left err -> Left err
    Right parsed -> parsed `deepseq` Right parsed

update :: Text -> v -> IORef (HashMap Text v) -> IO ()
update devId !val ref = modifyIORef' ref (HM.insert devId val)

statusMap :: Specification -> HashMap Text Text
statusMap spec = HM.fromList $ map go (Vec.toList $ specStatus spec)
 where
  go s = (Text.pack $ show $ statusDpId s, statusCode s)

ensurePoller :: Env -> Text -> IO ()
ensurePoller env devId = do
  pollers <- readIORef (envPollers env)
  case HM.lookup devId pollers of
    Just _ -> return ()
    Nothing -> startPoller env devId

startPoller :: Env -> Text -> IO ()
startPoller env devId = do
  a <- async (threadDelay 2000000 >> devicePoller env devId)
  mp <- atomicModifyIORef' (envPollers env) $ \pollers -> (HM.insert devId a pollers, HM.lookup devId pollers)
  case mp of
    Just p -> do
      putStrLn $ "Replacing poller for " <> show devId
      cancel p
    Nothing -> putStrLn $ "Started poller for " <> show devId

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
  vers <- readIORef (envVers env)
  keys <- readIORef (envKeys env)
  maps <- readIORef (envStatusMap env)
  case (HM.lookup devId ips, HM.lookup devId vers, HM.lookup devId keys, HM.lookup devId maps) of
    (Just ip, Just (Just ver), Just key, Just smap) -> do
      sockaddr <- sockAddrForIp ip
      bracket (connect 1000000 sockaddr ver key) (traverse_ close) $ \case
        Just c -> do
          t <- getT'
          v <- case ver of
            Tuya33 -> do
              sendCmd c DpQuery (GetDeviceStatus devId devId t devId)
              recvMsg @DeviceStatus 1000000 c
            Tuya34 -> do
              localKey <- getRandomBytes 16
              sendCmdBS c SessKeyNegStart localKey
              res <- recvBS 1000000 c
              case res of
                Left err -> error $ "recvBS failed: " <> err
                Right res' -> do
                  let
                    (remoteKey, expectedHmac) = BS.splitAt 16 (msgPayload res')
                    localHmac = convert $ hmacGetDigest $ sha256 key localKey
                    remoteHmac = convert $ hmacGetDigest $ sha256 key remoteKey
                  unless (localHmac == expectedHmac) $ fail "HMAC mismatch during session negotiation"
                  sendCmdBS c SessKeyNegFinish remoteHmac
                  let
                    xored = xor localKey remoteKey
                    sessionKey = ecbEncrypt (cipher key) xored
                  sendCmd' sessionKey c DpQueryNew (GetDeviceStatus devId devId t devId)
                  recvMsg' sessionKey 1000000 c
          case v of
            Left err -> error $ "recvMsg failed: " <> err
            Right msg -> do
              deviceStatus env devId smap (msgPayload msg) -- showStatus env devId smap (msgPayload msg)
        Nothing -> Text.putStrLn $ devId <> " connect time out"
      threadDelay 10000000
    _ -> threadDelay 10000000
 where
  getT' = Text.pack . formatTime defaultTimeLocale "%s" <$> getCurrentTime

sha256 :: BS.ByteString -> BS.ByteString -> HMAC SHA256
sha256 = hmac

cipher :: BS.ByteString -> AES128
cipher = throwCryptoError . cipherInit

data GetDeviceStatus = GetDeviceStatus
  { gdsGwId :: Text
  , gdsDevId :: Text
  , gdsT :: Text
  , gdsUid :: Text
  }
  deriving (Show)

data SetDevice = SetDevice
  { sdGwId :: Text
  , sdDevId :: Text
  , sdDps :: DataPoints
  , sdT :: Text
  , sdUid :: Text
  }
  deriving (Show)

newtype DeviceStatus = DeviceStatus
  { dsDps :: DataPoints
  }
  deriving (Show)

newtype DataPoints = DataPoints {unDatapoints :: Object}
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

instance ToJSON SetDevice where
  toJSON SetDevice{..} =
    object
      [ "gwId" .= sdGwId
      , "devId" .= sdDevId
      , "t" .= sdT
      , "dps" .= sdDps
      , "uid" .= sdUid
      ]

instance FromJSON DeviceStatus where
  parseJSON =
    withObject "DeviceStatus" $ \o ->
      DeviceStatus <$> o .: "dps"

instance FromJSON DataPoints where
  parseJSON =
    withObject "DataPoints" (pure . DataPoints)

instance ToJSON DataPoints where
  toJSON (DataPoints dps) = toJSON dps

showStatus :: Env -> Text -> HashMap Text Text -> DeviceStatus -> IO ()
showStatus _env devId smap status = do
  forM_ (KeyMap.toList (unDatapoints $ dsDps status)) $ \(dp, val) -> do
    case HM.lookup (Key.toText dp) smap of
      Just code -> do
        let Just topic = MQTT.mkTopic ("tuya/device/" <> devId <> "/status/" <> code)
        print (topic, val)
      Nothing -> return ()

deviceStatus :: Env -> Text -> HashMap Text Text -> DeviceStatus -> IO ()
deviceStatus env devId smap status = do
  forM_ (KeyMap.toList (unDatapoints $ dsDps status)) $ \(dp, val) -> do
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
