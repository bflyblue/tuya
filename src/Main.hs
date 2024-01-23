{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import System.Environment

import Tuya.Config
import Tuya.Devices
import Tuya.Discover
import Tuya.HomeAssistant
import Tuya.Poll

main :: IO ()
main = do
  args <- getArgs
  let configPath = case args of
                    [] -> "tuya.yaml"
                    (path:_) -> path
  cfg <- readConfigFile configPath
  discover (cfgMqtt cfg)
    `concurrently_` serve cfg
    `concurrently_` poller cfg
    `concurrently_` homeAssist cfg
