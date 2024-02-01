{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Tuya.Decode where

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types
import Crypto.Data.Padding
import Crypto.Error
import Data.ByteString as BS
import Data.Maybe
import Data.Serialize.Get

import Tuya.Types

decode :: Protocol -> ByteString -> ByteString -> Either String (Msg ByteString)
decode Tuya33 = decode33
decode Tuya34 = decode34

decode33 :: ByteString -> ByteString -> Either String (Msg ByteString)
decode33 key bs = do
  Raw{..} <- runGet getRaw33 bs
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

decode34 :: ByteString -> ByteString -> Either String (Msg ByteString)
decode34 key bs = do
  Raw{..} <- runGet getRaw34 bs
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

getRaw33 :: Get Raw
getRaw33 = do
  rawPrefix <- getWord32be
  rawSequence <- getWord32be
  rawCommand <- getWord32be
  rawPayloadSize <- getWord32be
  rawReturnCode <- getWord32be
  rawPayload <- getByteString (fromIntegral rawPayloadSize - 12)
  rawCrc <- getWord32be
  rawSuffix <- getWord32be
  return Raw{..}

getRaw34 :: Get Raw
getRaw34 = do
  rawPrefix <- getWord32be
  rawSequence <- getWord32be
  rawCommand <- getWord32be
  rawPayloadSize <- getWord32be
  rawReturnCode <- getWord32be
  rawPayload <- getByteString (fromIntegral rawPayloadSize - 40)
  rawCrc <- getWord32be
  skip 28
  rawSuffix <- getWord32be
  return Raw{..}

decryptPayload :: ByteString -> ByteString -> ByteString
decryptPayload key = fromMaybe (error "unpad") . unpad (PKCS7 (blockSize cipher)) . ecbDecrypt cipher
 where
  cipher :: AES128
  cipher = throwCryptoError $ cipherInit key
