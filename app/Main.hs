module Main where

import Data.Owoify.Owoify (owoify, OwoifyLevel(..))
import Data.Text.Lazy (pack, unpack)

main :: IO ()
main = do
  owoResult <- owoify (pack "This is the string to owo! Kinda cute, isn't it?") Owo
  uvuResult <- owoify (pack "This is the string to owo! Kinda cute, isn't it?") Uvu
  helloWorld <- owoify (pack "Hello World") Owo
  putStrLn $ unpack owoResult
  putStrLn $ unpack uvuResult
  putStrLn $ unpack helloWorld