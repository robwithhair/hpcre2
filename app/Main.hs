{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Regex.PCRE2.Wrap
import qualified Data.ByteString as B

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
                 possibleBytestring <- serializeRegexs [r]
                 case possibleBytestring of
                      (Left error) -> do
                            putStrLn $ "Error creating Bytestring" ++ show error
                      (Right toWrite) -> do
                            B.writeFile "test.regexs" toWrite
                            putStrLn "Wrote file successfully"
                 putStrLn $ show matches
