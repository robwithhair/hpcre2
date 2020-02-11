{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Regex.PCRE2.Wrap

main :: IO ()
main = do
     regex <- compileRegex "\\w*"
     case regex of
          (Left error) -> do
                putStrLn "Left"
                putStrLn (show error)
          (Right r) -> do
                 putStrLn "Right"
                 matches <- match r " "
                 putStrLn $ show matches
