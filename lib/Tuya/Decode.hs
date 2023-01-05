{-# LANGUAGE RecordWildCards #-}

module Tuya.Decode where

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types
import Crypto.Data.Padding
import Crypto.Error
import Data.ByteString as BS
import Data.Maybe
import Data.Serialize.Get

import Tuya.Types

decode :: ByteString -> ByteString -> Either String (Msg ByteString)
decode key bs = do
  Raw{..} <- decodeRaw bs
  if rawPrefix == 0x55aa && rawSuffix == 0xaa55
    then do
      let decrypted = decryptPayload key rawPayload
      pure
        Msg
          { msgSequence = rawSequence
          , msgCommand = toEnum (fromIntegral rawCommand)
          , msgReturnCode = rawReturnCode
          , msgPayload = decrypted
          }
    else Left "Prefix or Suffix was incorrect"

decodeRaw :: ByteString -> Either String Raw
decodeRaw = runGet getRaw

getRaw :: Get Raw
getRaw = do
  rawPrefix <- getWord32be
  rawSequence <- getWord32be
  rawCommand <- getWord32be
  rawPayloadSize <- getWord32be
  rawReturnCode <- getWord32be
  rawPayload <- getByteString (fromIntegral rawPayloadSize - 12)
  rawCrc <- getWord32be
  rawSuffix <- getWord32be
  return Raw{..}

decryptPayload :: ByteString -> ByteString -> ByteString
decryptPayload key = fromMaybe (error "unpad") . unpad (PKCS7 (blockSize cipher)) . ecbDecrypt cipher
 where
  cipher :: AES128
  cipher = throwCryptoError $ cipherInit key