{-# LANGUAGE OverloadedStrings #-}

module Main where

import Tuya.Config
import Tuya.Poll

main :: IO ()
main = do
  cfg <- readConfigFile "tuya.yaml"
  poller cfg