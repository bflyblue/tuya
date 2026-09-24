{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Tuya.Discover where

import Control.Monad (forever)
import Crypto.Hash (Digest, MD5, hash)
import qualified Data.Aeson as Aeson
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import qualified Network.MQTT.Client as MQTT
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as Sock

import Tuya.Config
import Tuya.Decode
import Tuya.Mqtt
import Tuya.Types

discoverKey :: ByteString
discoverKey = convert (md5 "yGAdlopoPVldABfn")
 where
  md5 :: ByteString -> Digest MD5
  md5 = hash

discover :: MqttSettings -> IO ()
discover mqttSettings = do
  mc <- mqttClient mqttSettings
  s <- udpSocket 6667
  forever $ do
    (bs, _f) <- Sock.recvFrom s 65536
    case decode Tuya33 discoverKey bs of
      Left err -> putStrLn $ "discover: ignoring packet: " <> err
      Right m ->
        case Aeson.eitherDecodeStrict' (msgPayload m) of
          Left err -> putStrLn $ "discover: ignoring packet: " <> err
          Right gw -> publishDevice mc (gwGwId gw) ["discover"] (fromStrict (msgPayload m)) False

mqttClient :: MqttSettings -> IO MQTT.MQTTClient
mqttClient settings =
  MQTT.connectURI
    MQTT.mqttConfig{MQTT._protocol = mqttProtocol settings}
    (mqttBrokerUri settings)

udpSocket :: Sock.PortNumber -> IO Sock.Socket
udpSocket port = do
  s <- Sock.socket Sock.AF_INET Sock.Datagram Sock.defaultProtocol
  Sock.bind s (Sock.SockAddrInet port (Sock.tupleToHostAddress (0, 0, 0, 0)))
  return s
