{-# LANGUAGE OverloadedStrings #-}
import Stock
import HTTPObjects
import System.Environment
import Data.Map
import Data.Text
import Data.Functor.Identity
import Database.CQL.IO as Client
import qualified System.Logger as Logger
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time
import Database.CQL.Protocol
import System.IO
import Data.ByteString as B

file = "./resources/stock_list"

x = Keyspace { unKeyspace = "dev" }
settings = setKeyspace x defSettings
p = defQueryParams One ()
main :: IO ()
main = do 
  let symbol = "AAPL"
  h <- openFile "./resources/stock_list" ReadMode
  logger <- Logger.new (Logger.setOutput Logger.StdOut Logger.defSettings)
  infoList <- B.hGetContents h >>= mapM (getMostRecentInfo "TIME_SERIES_DAILY") >>= logInfo logger
  x <- getArgs
  let output = getArg x  
  let (timeString, info) = infoList !! 0
  let time = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" timeString :: UTCTime
  let q = "insert into aapl (uuid, close, date_collected, high, low, open, volume) values(uuid(), ?, ?, ?, ?, ?, ?)" :: QueryString W (Double, UTCTime, Double, Double, Double, Integer) ()
  let params = defQueryParams One $ infoToParams info time
  c <- Client.init settings
  runClient c (write q params)

  hClose h
  Logger.flush logger
  Logger.close logger

infoToParams :: Info -> UTCTime -> (Double, UTCTime, Double, Double, Double, Integer)
infoToParams (Info h o v c l) time = (read c, time, read h, read l, read o, read v)

logInfo :: Show a => Logger.Logger -> a -> IO a
logInfo l message = do
  Logger.log l Logger.Info (Logger.msg $ show message)
  Logger.flush l
  return message

getArg :: [String] -> String
getArg [] = ""
getArg x = Prelude.unlines x
