import Stock
import System.Environment

file = "./resources/stock_list"

main :: IO ()
main = do 
  x <- getArgs
  let output = getArg x  
  f <- readFile "./resources/stock_list"
  putStrLn "Hello"
  putStrLn f
  appendFile file output

getArg :: [String] -> String
getArg [] = ""
getArg x = unlines x
