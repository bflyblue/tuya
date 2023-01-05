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

encode33 :: Aeson.ToJSON a => ByteString -> Msg a -> ByteString
encode33 key Msg{..} = runPut (putRaw (raw msgCommand msgSequence payload))
 where
  bs = encryptPayload key $ BSL.toStrict (Aeson.encode msgPayload)

  payload = case msgCommand of
    DpQuery -> bs
    DpRefresh -> bs
    _ -> extendedHeader <> bs

  extendedHeader = "3.3" <> BS.replicate 12 0

raw :: CommandType -> Word32 -> ByteString -> Raw
raw cmd seqno payload =
  Raw
    { rawPrefix = 0x55aa
    , rawSequence = seqno
    , rawCommand = fromIntegral (fromEnum cmd)
    , rawPayloadSize = fromIntegral (BS.length payload) + 8
    , rawReturnCode = 0
    , rawPayload = payload
    , rawCrc = 0
    , rawSuffix = 0xaa55
    }

putRaw :: Putter Raw
putRaw Raw{..} = do
  checksum <- putCRC32 $ do
    putWord32be rawPrefix
    putWord32be rawSequence
    putWord32be rawCommand
    putWord32be rawPayloadSize
    putByteString rawPayload
  putWord32be checksum
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