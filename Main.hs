{-# LANGUAGE OverloadedStrings #-}
import Stock
import HTTPObjects
import System.Environment
import Data.Map
import Data.Functor.Identity
import Database.CQL.IO as Client
import qualified System.Logger as Logger
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time
import Database.CQL.Protocol
import System.IO
import Data.Text.Lazy as T

file = "./resources/stock_list"

x = Keyspace { unKeyspace = "dev" }
settings = setKeyspace x defSettings
p = defQueryParams One ()

main :: IO ()
main = do 
  h <- openFile "./resources/stock_list" ReadMode
  logger <- Logger.new (Logger.setOutput Logger.StdOut Logger.defSettings)

  symbols <- hGetContents h 
  let symbolList = Prelude.lines symbols
  logInfo logger symbolList
  stockData <- mapM (getMostRecentInfo "TIME_SERIES_DAILY") symbolList
  let zipped = Prelude.zip symbolList stockData
  let databaseInfo = Prelude.map (\(sym, (time, info)) -> (createQuery sym, createParams time info)) zipped
  logInfo logger databaseInfo
  c <- Client.init settings
  mapM (\(query, params) -> runClient c (write query params)) databaseInfo

  hClose h
  Logger.flush logger
  Logger.close logger

createParams time info = defQueryParams One $ infoToParams info (parseT time)

parseT time = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" time :: UTCTime

createQuery :: String -> QueryString W (Double, UTCTime, Double, Double, Double, Integer) ()
createQuery sym = QueryString { unQueryString = T.pack ("insert into " ++ sym ++ " (uuid, close, date_collected, high, low, open, volume) values(uuid(), ?, ?, ?, ?, ?, ?)") }

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
