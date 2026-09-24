{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Tuya.Mqtt where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.MQTT.Client as MQTT
import qualified Network.MQTT.Topic as MQTT

-- | Build a topic from its levels. Fails if any level is empty or contains a
-- separator or wildcard, which would otherwise yield a wrong or invalid topic.
topicFromLevels :: [Text] -> Maybe MQTT.Topic
topicFromLevels levels
  | all validLevel levels = MQTT.mkTopic (Text.intercalate "/" levels)
  | otherwise = Nothing
 where
  validLevel l = not (Text.null l) && not (Text.any (`elem` ['/', '+', '#', '\0']) l)

deviceTopic :: Text -> [Text] -> Maybe MQTT.Topic
deviceTopic devId levels = topicFromLevels ("tuya" : "device" : devId : levels)

publishLevels :: MQTT.MQTTClient -> [Text] -> ByteString -> Bool -> IO ()
publishLevels mc levels payload retain =
  case topicFromLevels levels of
    Just topic -> MQTT.publish mc topic payload retain
    Nothing -> putStrLn $ "refusing to publish to invalid topic " <> show (Text.intercalate "/" levels)

publishDevice :: MQTT.MQTTClient -> Text -> [Text] -> ByteString -> Bool -> IO ()
publishDevice mc devId levels = publishLevels mc ("tuya" : "device" : devId : levels)
