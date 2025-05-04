{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async (concurrently_)
import Tuya.Config
import Tuya.Devices (serve)
import Tuya.Discover (discover)
import Tuya.HomeAssistant (homeAssist)
import Tuya.Poll (poller)

infixl 9 //
(//) :: IO a -> IO b -> IO ()
(//) = concurrently_

main :: IO ()
main = do
  cfg <- readConfigFile "tuya.yaml"
  discover (cfgMqtt cfg) // serve cfg // poller cfg // homeAssist cfg