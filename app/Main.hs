{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Regex.PCRE2.Wrap

main :: IO ()
main = do
     regex <- compileRegex "\\b((?<! not | not have | not a | not the | not be | not really | not too | not as | not had | not even | not feel | not been | not have to | not so | not more | not seem | not have a | not seem to | not very | not that | not have any | not much | not having | not what | not have the | not an | not had any | not have been | not at all | not ever | not particularly | or | or so | or a | or the | or even | or just | or to | or anything | or have | or as | or too | or that | or get | or are | or very | or feel | no | no more | no longer | no longer have | no further | no real | no noticeable | no such | no apparent | no obvious | dont | dont have | dont be | dont really | dont even | dont feel | dont seem | dont seem to | dont ever | doesnt | doesnt have | doesnt be | doesnt really | doesnt even | doesnt feel | doesnt seem | doesnt seem to | doesnt ever | didnt | didnt have | didnt be | didnt really | didnt even | didnt feel | didnt seem | didnt seem to | didnt ever | nothing | nothing was | nothing has | nothing is | nothing really | nothing ever | nothing seemed | nothing seemed to | never | never had | never been | never have | never felt | never had a | never really | never even | without | without a | without the | without being | without having | without really | without even | without ever | without actually | cant | cant have | cant be | cant really | cant even | cant feel | cant seem | cant seem to | cant ever | any | any more | any longer | any longer have | any further | any real | any noticeable | any such | any apparent | any obvious | havent | havent the | havent really | havent had | havent even | havent been | havent had a | havent felt | couldnt | couldnt have | couldnt be | couldnt really | couldnt even | couldnt feel | couldnt seem | couldnt seem to | couldnt ever | cannot | cannot have | cannot be | cannot really | cannot even | cannot feel | cannot seem | cannot seem to | cannot ever | wasnt | wasnt a | wasnt the | wasnt really | wasnt too | wasnt as | wasnt even | wasnt so | isnt | isnt a | isnt the | isnt really | isnt too | isnt as | isnt even | isnt so | wont | wont have | wont be | wont really | wont even | wont feel | wont seem | wont seem to | wont ever | wouldnt | wouldnt have | wouldnt be | wouldnt really | wouldnt even | wouldnt feel | wouldnt seem | wouldnt seem to | wouldnt ever | arent | arent a | arent the | arent really | arent too | arent as | arent even | arent so | hasnt | hasnt the | hasnt really | hasnt had | hasnt even | hasnt been | hasnt had a | hasnt felt | hadnt | hadnt the | hadnt really | hadnt had | hadnt even | hadnt been | hadnt had a | hadnt felt | werent | werent a | werent the | werent really | werent too | werent as | werent even | werent so | arnt | arnt a | arnt the | arnt really | arnt too | arnt as | arnt even | arnt so | un )best so far)\\b"
     case regex of
          (Left error) -> do
                putStrLn "Left"
                putStrLn (show error)
          (Right r) -> do
                 putStrLn "Right"
                 matches <- match r "this is the best so far I'm sure"
                 putStrLn $ show matches
