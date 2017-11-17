{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
#-}

module Infra where

import qualified Data.Map as M
import Control.Monad.State as S
import Network.Socket as NS

data Message = JoinRoom Int
             | TextData String
             | NickName String
             | Disconnect
    deriving (Show, Eq)

data Address = Address {ip :: NS.HostName, port :: NS.ServiceName}

data Chatroom = Chatroom {participates :: M String ClientStore}

data ClientStore = ClientStore {nickName :: String, clientAddr :: Address, roomNum :: Int}

data ServerStore = ServerStore {chatRooms :: [Chatroom], serverAddrs :: [Address]}
