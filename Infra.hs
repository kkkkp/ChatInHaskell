{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
#-}

module Infra where

import Network.Socket as NS

data Message = Join Int
             | Text String
             | Nick String
             | Part
             | Quit
    deriving (Show, Eq)

data Address = Address {ip :: NS.HostName, port :: NS.ServiceName}

readFrom :: Address -> IO Message
readFrom addr = undefined

sendTo :: Address -> Message -> IO ()
sendTo addr msg = undefined
