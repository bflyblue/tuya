{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tuya.Encode where

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types
import Crypto.Data.Padding
import Crypto.Error
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Serialize.Put

import Data.Digest.CRC32 (crc32)
import Data.Word (Word32)
import Tuya.Types

encode :: Aeson.ToJSON a => Protocol -> ByteString -> Msg a -> ByteString
encode Tuya33 = encode33
encode Tuya34 = encode34

encode33 :: Aeson.ToJSON a => ByteString -> Msg a -> ByteString
encode33 key Msg{..} = runPut (putRaw33 (raw msgCommand msgSequence payload))
 where
  bs = encryptPayload key $ BSL.toStrict (Aeson.encode msgPayload)

  payload = case msgCommand of
    DpQuery -> bs
    DpRefresh -> bs
    _ -> extendedHeader <> bs

  extendedHeader = "3.3" <> BS.replicate 12 0

encode34 :: Aeson.ToJSON a => ByteString -> Msg a -> ByteString
encode34 key Msg{..} = runPut (putRaw34 (raw msgCommand msgSequence payload))
 where
  bs = encryptPayload key $ BSL.toStrict (Aeson.encode msgPayload)

  payload = case msgCommand of
    DpQuery -> padded bs
    DpQueryNew -> padded bs
    DpRefresh -> padded bs
    HeartBeat -> padded bs
    SessKeyNegStart -> padded bs
    SessKeyNegFinish -> padded bs
    _ -> padded (extendedHeader <> bs)

  extendedHeader = "3.4" <> BS.replicate 12 0
  padded x = let padding = 16 - (BS.length x `mod` 16) in x <> BS.replicate padding 0

raw :: CommandType -> Word32 -> ByteString -> Raw
raw cmd seqno payload =
  Raw
    { rawPrefix = 0x55aa
    , rawSequence = seqno
    , rawCommand = fromIntegral (fromEnum cmd)
    , rawPayloadSize = fromIntegral (BS.length payload)
    , rawReturnCode = 0
    , rawPayload = payload
    , rawCrc = 0
    , rawSuffix = 0xaa55
    }

putRaw33 :: Putter Raw
putRaw33 Raw{..} = do
  checksum <- putCRC32 $ do
    putWord32be rawPrefix
    putWord32be rawSequence
    putWord32be rawCommand
    putWord32be (rawPayloadSize + 8)
    putByteString rawPayload
  putWord32be checksum
  putWord32be rawSuffix

putRaw34 :: Putter Raw
putRaw34 Raw{..} = do
  checksum <- putCRC32 $ do
    putWord32be rawPrefix
    putWord32be rawSequence
    putWord32be rawCommand
    putWord32be (rawPayloadSize + 36)
    putByteString rawPayload
  putWord32be checksum
  putByteString (BS.replicate 28 0)
  putWord32be rawSuffix

putCRC32 :: Put -> PutM Word32
putCRC32 p = do
  let bs = runPut p
  putByteString bs
  return (crc32 bs)

encryptPayload :: ByteString -> ByteString -> ByteString
encryptPayload key = ecbEncrypt cipher . pad (PKCS7 (blockSize cipher))
 where
  cipher :: AES128
  cipher = throwCryptoError $ cipherInit key
