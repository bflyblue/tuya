{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async

import Tuya.Config
import Tuya.Devices
import Tuya.Discover
import Tuya.HomeAssistant
import Tuya.Poll

main :: IO ()
main = do
  cfg <- readConfigFile "tuya.yaml"
  poller cfg

-- discover (cfgMqtt cfg)
--   `concurrently_` serve cfg
--   `concurrently_` poller cfg
--   `concurrently_` homeAssist cfg