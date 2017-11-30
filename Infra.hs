{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
#-}

module Infra where

import Network.Socket as NS

data Message = Join Int
             | Text String
             | STxt Int String
             | Nick String
             | Part
             | Quit
    deriving (Show, Eq)

-- readFrom :: SockAddr -> IO Message
-- readFrom addr = undefined
--
-- sendTo :: SockAddr -> Message -> IO ()
-- sendTo addr msg = undefined
