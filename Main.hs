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

file = "./resources/stock_list"

x = Keyspace { unKeyspace = "dev" }
settings = setKeyspace x defSettings
p = defQueryParams One ()
main :: IO ()
main = do 
  let symbol = "AAPL"
  logger <- Logger.new (Logger.setOutput Logger.StdOut Logger.defSettings)
  infoList <- getMostRecentInfo symbol "TIME_SERIES_DAILY"
  Logger.log logger Logger.Info (Logger.msg $ show (infoList !! 0))
  x <- getArgs
  let output = getArg x  
  f <- readFile "./resources/stock_list"
  appendFile file output
  let (timeString, info) = infoList !! 0
  let time = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" timeString :: UTCTime
  let q = "insert into aapl (uuid, close, date_collected, high, low, open, volume) values(uuid(), ?, ?, ?, ?, ?, ?)" :: QueryString W (Double, UTCTime, Double, Double, Double, Integer) ()
  let params = defQueryParams One $ infoToParams info time
  c <- Client.init settings
  runClient c (write q params)
  Logger.flush logger
  Logger.close logger

infoToParams :: Info -> UTCTime -> (Double, UTCTime, Double, Double, Double, Integer)
infoToParams (Info h o v c l) time = (read c, time, read h, read l, read o, read v)

getArg :: [String] -> String
getArg [] = ""
getArg x = Prelude.unlines x
