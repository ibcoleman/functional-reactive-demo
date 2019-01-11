module Lib
    ( someFunc,
      myReverse
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

myReverse :: [a] -> [a]
myReverse []      = []
myReverse [x]     = [x]
myReverse (x:xs)  = (myReverse xs) ++ [x]


