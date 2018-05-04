module Main where

import Control.Monad.IO.Class
import Control.Monad.State.Lazy

main :: IO ()
main = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn $ "hi " ++ name

altMain =
  putStrLn "What's your Name" >> getLine >>= \n -> putStrLn $ "hi " ++ n

type MyMonad = StateT Integer IO

doStuff :: MyMonad String
doStuff = do
  liftIO $ putStrLn "What's your name?"
  name <- liftIO getLine
  modify (+1)
  liftIO $ putStrLn $ "hi " ++ name ++ ", what's your favorite color?"
  color <- liftIO getLine
  modify (+1)
  return $ "name: " ++ name ++ ", color: " ++ color
