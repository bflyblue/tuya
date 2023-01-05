{-# LANGUAGE RecordWildCards #-}

module Tuya.Decode where

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types
import Crypto.Data.Padding
import Crypto.Error
import qualified Data.Aeson as Aeson
import Data.ByteString as BS
import Data.Maybe
import Data.Serialize.Get

import Tuya.Types

decode :: ByteString -> ByteString -> Either String Msg
decode key bs = do
  Raw{..} <- decodeRaw bs
  if prefix == 0x55aa && suffix == 0xaa55
    then do
      value <- Aeson.eitherDecodeStrict' (decryptPayload key payload)
      pure
        Msg
          { msgSequence = sequence'
          , msgCommand = toEnum (fromIntegral command)
          , msgReturnCode = returnCode
          , msgPayload = value
          }
    else Left "Prefix or Suffix was incorrect"

decodeRaw :: ByteString -> Either String Raw
decodeRaw = runGet getRaw

getRaw :: Get Raw
getRaw = do
  prefix <- getWord32be
  sequence' <- getWord32be
  command <- getWord32be
  payloadSize <- getWord32be
  returnCode <- getWord32be
  payload <- getByteString (fromIntegral payloadSize - 12)
  crc <- getWord32be
  suffix <- getWord32be
  return Raw{..}

decryptPayload :: ByteString -> ByteString -> ByteString
decryptPayload key = fromMaybe (error "unpad") . unpad (PKCS7 (blockSize cipher)) . ecbDecrypt cipher
 where
  cipher :: AES128
  cipher = throwCryptoError $ cipherInit key