module Main where

import Prelude hiding (catch)
import Control.Exception
import System.Environment
import System.IO.Error
import System.IO
import Data.Maybe
import Lib
import DataTypes

main :: IO ()
main = do
  handle <- openFile "traf.txt" ReadMode
  fileContents <- hGetContents handle
  let fLine = lines fileContents -- разбиваем файл в список строк
      rawTransmit = map parseLine fLine -- преобразовываем в список Maybe векторов
      trS = map fromJust $ filter (\x -> x /= Nothing) rawTransmit -- убираем Nothing-записи и извлекаем векторы из Just
      newContents = trCase1 trS ++ trCase2 trS ++ trCase3 trS ++ trCase4 trS ++ trCase5 trS ++ "\n 6. Proxy problem not solved" -- trCase6 trS
  writeFile "result.txt" newContents

parseLine :: String -> Maybe Transmit
-- парсим строку в вектор передачи
parseLine x
  | length splitted /= 7 = Nothing
  | otherwise = makeTransmit trIP rcIP isUDP bytes time
  where
    splitted = splitBy ';' x
    trIP = toIP (splitted !! 0)
    rcIP = toIP (splitted !! 2)
    isUDP = (splitted !! 4) == "true"
    bytes = read (splitted !! 5)
    time = read (splitted !! 6)
