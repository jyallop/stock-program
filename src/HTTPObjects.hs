{-# LANGUAGE OverloadedStrings #-}
module HTTPObjects where
import Data.Aeson
import Data.Map
import Data.Text
import Data.Aeson
import Network.HTTP.Simple
import qualified System.Logger as Logger
import Data.ByteString

getMostRecentInfo :: ByteString -> ByteString -> IO [(String, Info)]
getMostRecentInfo symbol series = do
  logger <- Logger.new (Logger.setOutput Logger.StdOut Logger.defSettings)
  request' <- parseRequest "GET https://www.alphavantage.co"
  let request
          = setRequestMethod "GET"
          $ setRequestPath "/query"
          $ setRequestQueryString [("function", Just series), ("symbol", Just symbol), ("apikey",Just "GB563O12AR9KLWXD")]
          $ setRequestSecure True
          $ request'

  Logger.log logger Logger.Info (Logger.msg $ show request)
  response <- httpJSON request
  Logger.log logger Logger.Info (Logger.msg $ "The status code was: " ++ show (getResponseStatusCode response))

  let body = fromJSON $ getResponseBody response :: Result Resp
  let info = getTimeSeries $ pullOutResponse body
  Logger.flush logger
  Logger.close logger
  return $ toDescList info

pullOutResponse :: Result Resp -> Resp
pullOutResponse (Success x) = x
pullOutResponse (Error x) = error x

data Resp = Resp {
  metaData :: Object,
  timeSeries :: Map String Info
} deriving Show

getTimeSeries :: Resp -> Map String Info
getTimeSeries (Resp _ info) = info

instance FromJSON Resp where
    parseJSON = withObject "Resp" $ \v -> Resp
        <$> v .: "Meta Data"
        <*> v .: "Time Series (Daily)"

data Info = Info {
   high :: String,
   open :: String,
   volume :: String,
   close :: String,
   low :: String
} deriving (Show)

instance FromJSON Info where
    parseJSON = withObject "Info" $ \v -> Info
        <$> v .: "2. high"
        <*> v .: "1. open"
        <*> v .: "5. volume"
        <*> v .: "4. close"
        <*> v .: "3. low"
