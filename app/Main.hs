{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Regex.PCRE2.Wrap

main :: IO ()
main = do
     regex <- compileRegex "best (so) far"
     case regex of
          (Left error) -> do
                putStrLn "Left"
                putStrLn (show error)
          (Right r) -> do
                 putStrLn "Right"
                 matches <- match r "this is the best so far I'm sure.  Yes it's definately the best so far for sure."
                 putStrLn $ show matches
