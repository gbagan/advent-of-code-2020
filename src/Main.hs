module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Day01 (solve)
import qualified Day02 (solve)
import System.Environment (getArgs)

solutions :: Map String (String -> IO())
solutions = Map.fromList
            [   ("01", Day01.solve)
            ,   ("02", Day02.solve)
            ]

solveProblem :: String -> IO ()
solveProblem name = case Map.lookup name solutions of
    Just solve -> do
        putStrLn $ "Solve day " ++ name
        s <- readFile ("./data/data" ++ name)
        solve s
    Nothing -> putStrLn $ "Day not implemented: " ++ name

main :: IO ()
main = do
    args <- getArgs
    mapM_ solveProblem if null args then Map.keys solutions else args