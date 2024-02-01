{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Tuya.Cloud where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Crypto.Hash
import Crypto.MAC.HMAC
import Data.Aeson hiding (Result)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (foldedCase)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX
import Data.Time.Format
import Network.HTTP.Client (Request, method, path, queryString, requestHeaders)
import Network.HTTP.Req

import Tuya.Types

newtype Cloud a = Cloud {unCloud :: ReaderT (HttpConfig, CloudAuth, Maybe ByteString) IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadHttp Cloud where
  getHttpConfig :: Cloud HttpConfig
  getHttpConfig = Cloud $ asks (\(cfg, _, _) -> cfg)

  handleHttpException :: HttpException -> Cloud a
  handleHttpException = Cloud . lift . throwIO

data CloudAuth = CloudAuth
  { clientId :: ByteString
  , clientSecret :: ByteString
  , appUserId :: Text
  }

runCloud :: MonadIO m => CloudAuth -> Cloud a -> m a
runCloud = runCloud' defaultHttpConfig

runCloud' :: MonadIO m => HttpConfig -> CloudAuth -> Cloud a -> m a
runCloud' config auth (Cloud m) = do
  liftIO $ do
    accessToken <- runReaderT (unCloud cloudAccessToken) (config, auth, Nothing)
    runReaderT m (config, auth, Just (encodeUtf8 accessToken))

cloudAccessToken :: Cloud Text
cloudAccessToken = do
  let opts =
        mconcat
          [ "grant_type" =: (1 :: Int)
          , header "mode" "cors"
          , header "content-type" "application/json"
          ]
  r <- reqCb GET (https "openapi.tuyaeu.com" /: "v1.0" /: "token") NoReqBody jsonResponse opts signReq
  return (tAccessToken $ getResult $ responseBody r)

sign :: ByteString -> ByteString -> Maybe ByteString -> ByteString -> ByteString -> ByteString -> ByteString
sign key clientid accessToken t nonce s = hex $ convert $ hmacGetDigest $ sha256 key (clientid <> fromMaybe mempty accessToken <> t <> nonce <> s)
 where
  sha256 :: ByteString -> ByteString -> HMAC SHA256
  sha256 = hmac

  hex = BS.map toUpper . Base16.encode

signReq :: Request -> Cloud Request
signReq r = do
  t <- getT
  (_, auth, accessToken) <- Cloud ask
  return $
    r
      { requestHeaders = signHeaders t (clientId auth) (clientSecret auth) accessToken ++ requestHeaders r
      }
 where
  signHeaders t clientid clientsecret accessToken =
    maybe [] (\tok -> [("access_token", tok)]) accessToken
      ++ [ ("sign", sign clientsecret clientid accessToken t "" (stringToSign []))
         , ("sign_method", "HMAC-SHA256")
         , ("client_id", clientid)
         , ("t", t)
         ]

  stringToSign signatureHeaders = BS.intercalate "\n" [method r, noContentSha256, headerBS signatureHeaders (requestHeaders r), url]
  headerBS signatureHeaders h = BS.intercalate "\n" [foldedCase key <> ":" <> val | (key, val) <- h, key `elem` signatureHeaders]
  noContentSha256 = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  url = path r <> queryString r

newtype Result a = Result {getResult :: a}
  deriving (Show)

newtype Token = Token {tAccessToken :: Text}
  deriving (Show)

data DeviceList = DeviceList
  { dlHasMore :: Bool
  , dlLastRowKey :: Text
  , dlList :: [Device]
  }
  deriving (Show)

{- FOURMOLU_DISABLE -}
instance FromJSON a => FromJSON (Result a) where
  parseJSON =
    withObject "Result" $ \w -> do
      Result <$> w .: "result"

instance FromJSON Token where
  parseJSON =
    withObject "Token" $ \v ->
      Token
        <$> v .: "access_token"

instance FromJSON DeviceList where
  parseJSON =
    withObject "DeviceList" $ \v ->
      DeviceList
        <$> v .: "has_more"
        <*> v .: "last_row_key"
        <*> v .: "list"

{- FOURMOLU_ENABLE -}

getT :: MonadIO m => m ByteString
getT = liftIO $ do
  now <- liftIO getPOSIXTime
  return $ BS.pack (formatTime defaultTimeLocale "%s000" now)

getDeviceList :: Cloud [Device]
getDeviceList = do
  (_, auth, _) <- Cloud ask
  getDeviceList' (appUserId auth) Nothing
 where
  getDeviceList' :: Text -> Maybe Text -> Cloud [Device]
  getDeviceList' tuyaUser lastRowKey = do
    let opts =
          mconcat
            [ "source_id" =: tuyaUser
            , "source_type" =: ("tuyaUser" :: Text)
            , maybe mempty ("last_row_key" =:) lastRowKey
            , header "mode" "cors"
            , header "content-type" "application/json"
            ]
    r <- reqCb GET (https "openapi.tuyaeu.com" /: "v1.3" /: "iot-03" /: "devices") NoReqBody jsonResponse opts signReq

    let dl = getResult (responseBody r)
    if dlHasMore dl
      then do
        r' <- getDeviceList' tuyaUser (Just $ dlLastRowKey dl)
        return $ dlList dl ++ r'
      else return $ dlList dl

getDevices :: [Text] -> Cloud [Device]
getDevices devs = do
  (_, auth, _) <- Cloud ask
  getDevice' (appUserId auth) Nothing
 where
  getDevice' :: Text -> Maybe Text -> Cloud [Device]
  getDevice' tuyaUser lastRowKey = do
    let opts =
          mconcat
            [ "device_ids" =: Text.intercalate "," devs
            , "source_id" =: tuyaUser
            , "source_type" =: ("tuyaUser" :: Text)
            , maybe mempty ("last_row_key" =:) lastRowKey
            , header "mode" "cors"
            , header "content-type" "application/json"
            ]
    r <- reqCb GET (https "openapi.tuyaeu.com" /: "v1.3" /: "iot-03" /: "devices") NoReqBody jsonResponse opts signReq

    let dl = getResult (responseBody r)
    if dlHasMore dl
      then do
        r' <- getDevice' tuyaUser (Just $ dlLastRowKey dl)
        return $ dlList dl ++ r'
      else return $ dlList dl

getDeviceSpecification1 :: Text -> Cloud Specification
getDeviceSpecification1 deviceid = do
  let opts =
        mconcat
          [ header "mode" "cors"
          , header "content-type" "application/json"
          ]
  r <- reqCb GET (https "openapi.tuyaeu.com" /: "v1.1" /: "devices" /: deviceid /: "specifications") NoReqBody jsonResponse opts signReq

  return $ getResult (responseBody r)

getDeviceSpecification2 :: Text -> Cloud Specification
getDeviceSpecification2 deviceid = do
  let opts =
        mconcat
          [ header "mode" "cors"
          , header "content-type" "application/json"
          ]
  r <- reqCb GET (https "openapi.tuyaeu.com" /: "v1.2" /: "iot-03" /: "devices" /: deviceid /: "specification") NoReqBody jsonResponse opts signReq

  return $ getResult (responseBody r)

getDeviceSpecification :: Text -> Cloud Specification
getDeviceSpecification deviceid = do
  spec1 <- getDeviceSpecification1 deviceid
  spec2 <- getDeviceSpecification2 deviceid
  return
    Specification
      { specCategory = specCategory spec1
      , specFunctions = zipWith stitchFunc (specFunctions spec1) (specFunctions spec2)
      , specStatus = zipWith stitchStatus (specStatus spec1) (specStatus spec2)
      }
 where
  stitchFunc f1 f2 =
    Function
      { funcCode = funcCode f1
      , funcDpId = funcDpId f1
      , funcName = funcName f2
      , funcType = funcType f1
      , funcValues = funcValues f1
      }
  stitchStatus s1 s2 =
    Status
      { statusCode = statusCode s1
      , statusDpId = statusDpId s1
      , statusName = statusName s2
      , statusType = statusType s1
      , statusValues = statusValues s1
      }
