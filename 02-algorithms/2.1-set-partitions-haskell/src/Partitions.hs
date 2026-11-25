module Main where

import Control.Monad (unless, when)
import Data.List (intercalate, nub)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import Text.Read (readMaybe)

data Opts = Opts
  { items :: [String]
  , size :: Maybe Int
  , blocks :: Maybe Int
  , onlyCount :: Bool
  }

partitions :: [a] -> [[[a]]]
partitions = go . reverse
  where
    go [] = [[]]
    go (x : xs) = concatMap (add x) (go xs)

add :: a -> [[a]] -> [[[a]]]
add x p =
  [take i p ++ [p !! i ++ [x]] ++ drop (i + 1) p | i <- [0 .. length p - 1]]
    ++ [p ++ [[x]]]

stirling :: Integer -> Integer -> Integer
stirling 0 0 = 1
stirling _ 0 = 0
stirling 0 _ = 0
stirling n k = k * stirling (n - 1) k + stirling (n - 1) (k - 1)

bell :: Integer -> Integer
bell n = sum [stirling n k | k <- [0 .. n]]

showPartition :: [[String]] -> String
showPartition [] = "{}"
showPartition p = intercalate ", " ["{" ++ intercalate ", " b ++ "}" | b <- p]

parse :: [String] -> Opts -> Either String Opts
parse [] o = Right o
parse ("-n" : v : rest) o = case readMaybe v of
  Just n | n >= 0 -> parse rest o {size = Just n}
  _ -> Left "n должно быть неотрицательным целым числом"
parse ("-k" : v : rest) o = case readMaybe v of
  Just k | k > 0 -> parse rest o {blocks = Just k}
  _ -> Left "k должно быть положительным целым числом"
parse ("-c" : rest) o = parse rest o {onlyCount = True}
parse [flag] _ | flag `elem` ["-n", "-k"] = Left ("после " ++ flag ++ " нужно число")
parse (x : rest) o = parse rest o {items = items o ++ [x]}

failWith :: String -> IO a
failWith msg = do
  hPutStrLn stderr ("Ошибка: " ++ msg)
  exitFailure

readItems :: IO [String]
readItems = do
  tty <- hIsTerminalDevice stdin
  when tty $ do
    putStr "элементы через пробел: "
    hFlush stdout
  eof <- isEOF
  if eof then return [] else words <$> getLine

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetEncoding stdin utf8
  args <- getArgs
  o <- either failWith return (parse args (Opts [] Nothing Nothing False))
  xs <- case (size o, items o) of
    (Just n, []) -> return (map show [1 .. n])
    (Just _, _) -> failWith "нельзя одновременно задавать -n и элементы"
    (Nothing, []) -> readItems
    (Nothing, ys) -> return ys
  when (length (nub xs) /= length xs) $ failWith "элементы повторяются"

  let ps = partitions xs
      result = maybe ps (\k -> filter ((== k) . length) ps) (blocks o)
      n = toInteger (length xs)
  unless (onlyCount o) $ mapM_ (putStrLn . showPartition) result
  putStrLn ("всего: " ++ show (length result))
  case blocks o of
    Nothing -> putStrLn ("число Белла B(" ++ show n ++ ") = " ++ show (bell n))
    Just k ->
      putStrLn
        ( "число Стирлинга S(" ++ show n ++ ", " ++ show k ++ ") = "
            ++ show (stirling n (toInteger k))
        )
