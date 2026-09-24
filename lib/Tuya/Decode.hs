{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Tuya.Decode where

import Control.Monad (when)
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types
import Crypto.Data.Padding
import Crypto.Error
import Data.ByteString as BS
import Data.Serialize.Get
import Data.Word (Word32)

import Tuya.Types

decode :: Protocol -> ByteString -> ByteString -> Either String (Msg ByteString)
decode Tuya33 = decode33
decode Tuya34 = decode34

decode33 :: ByteString -> ByteString -> Either String (Msg ByteString)
decode33 key bs = do
  Raw{..} <- runGet getRaw33 bs
  checkFraming rawPrefix rawSuffix
  cmd <- command rawCommand
  -- Some 3.3 frames carry an unencrypted "3.3" version header before the ciphertext.
  decrypted <- decryptPayload key (stripHeader "3.3" rawPayload)
  pure
    Msg
      { msgSequence = rawSequence
      , msgCommand = cmd
      , msgReturnCode = rawReturnCode
      , msgPayload = decrypted
      }

decode34 :: ByteString -> ByteString -> Either String (Msg ByteString)
decode34 key bs = do
  Raw{..} <- runGet getRaw34 bs
  checkFraming rawPrefix rawSuffix
  cmd <- command rawCommand
  decrypted <- decryptPayload key rawPayload
  pure
    Msg
      { msgSequence = rawSequence
      , msgCommand = cmd
      , msgReturnCode = rawReturnCode
      , msgPayload = decrypted
      }

checkFraming :: Word32 -> Word32 -> Either String ()
checkFraming prefix suffix
  | prefix == 0x55aa && suffix == 0xaa55 = Right ()
  | otherwise = Left "Prefix or Suffix was incorrect"

command :: Word32 -> Either String CommandType
command w = maybe (Left $ "Unknown command " ++ show w) Right (commandFromWord w)

stripHeader :: ByteString -> ByteString -> ByteString
stripHeader version payload
  | version `BS.isPrefixOf` payload = BS.drop (BS.length version + 12) payload
  | otherwise = payload

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

decryptPayload :: ByteString -> ByteString -> Either String ByteString
decryptPayload key payload = do
  cipher <- either (Left . show) Right $ eitherCryptoError (cipherInit key :: CryptoFailable AES128)
  when (BS.length payload `mod` blockSize cipher /= 0) $
    Left $ "Payload length " ++ show (BS.length payload) ++ " is not a multiple of the block size"
  maybe (Left "Invalid padding") Right $ unpad (PKCS7 (blockSize cipher)) (ecbDecrypt cipher payload)
