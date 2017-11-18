{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
#-}

module Client where

import Infra as Infra

-- should implement some kind of loop
-- how to realize select in C++?
--  in the case of the client need to wait for the server response
--  2 threads
runClient :: IO ()
runClient = undefined

main :: IO ()
main = runClient
