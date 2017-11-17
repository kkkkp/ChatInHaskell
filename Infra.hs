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

data ClientStore = ClientStore {nick :: String, room :: Int}

data Chatroom = Chatroom {usr :: M.Map Address ClientStore}

data ServerStore = ServerStore {grp :: M.Map Int Chatroom, getAddr :: M.Map Int Address}


-- register a server with id and addr
registerServer :: Int -> Address -> StateT ServerStore IO ()
registerServer n addr = do
    server <- get

    return ()

-- register a client with addr to grp 0 (defualt)
registerClient :: Address -> StateT ServerStore IO ()
registerClient = assignClient 0

-- assign a client to a different group
assignClient :: Int -> Address -> StateT ServerStore IO ()
assignClient n addr = undefined

-- disconnect a client from the group
partClient :: Int -> Address -> StateT ServerStore IO ()
partClient n addr = undefined

-- assign a client a nick name
nickClient :: String -> Address -> StateT ServerStore IO ()
nickClient nick addr = undefined
