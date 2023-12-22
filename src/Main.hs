{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async (concurrently_)
import Tuya.Config
import Tuya.Devices (serve)
import Tuya.Discover (discover)
import Tuya.HomeAssistant (homeAssist)
import Tuya.Poll (poller)

main :: IO ()
main = do
  cfg <- readConfigFile "tuya.yaml"
  -- poller cfg
  discover (cfgMqtt cfg)
    `concurrently_` serve cfg
    `concurrently_` poller cfg
    `concurrently_` homeAssist cfg