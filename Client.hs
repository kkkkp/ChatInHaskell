{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
#-}

module Client where

import Infra as Infra

-- how to handle invalid input?
-- use exceptT to handle invalid case?
getMessage :: String -> Infra.Message
getMessage input =
    case words input of
        "/join" : xs    -> Join 0
        "/text" : xs    -> Text $ unwords xs
        "/nick" : xs    -> Nick $ unwords xs
        "/part" : []    -> Part
        "/quit" : []    -> Quit
        _               -> Quit

runClient :: IO ()
runClient = undefined

main :: IO ()
main = runClient
