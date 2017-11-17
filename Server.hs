{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
#-}

module Server where

import Infra as Infra
import qualified Data.Map as M
import Control.Monad.State as S

data ClientStore = ClientStore {nick :: String, room :: Int}

data Chatroom = Chatroom {usr :: M.Map Infra.Address ClientStore}

data ServerStore = ServerStore {grp :: M.Map Int Chatroom, getAddr :: M.Map Int Infra.Address}


-- register a server with id and addr
registerServer :: Int -> Infra.Address -> StateT ServerStore IO ()
registerServer n addr = do
    server <- get

    return ()

-- register a client with addr to grp 0 (defualt)
registerClient :: Infra.Address -> StateT ServerStore IO ()
registerClient = assignClient 0

-- assign a client to a different group
assignClient :: Int -> Infra.Address -> StateT ServerStore IO ()
assignClient group addr = undefined

-- disconnect a client from the group
partClient :: Int -> Infra.Address -> StateT ServerStore IO ()
partClient group addr = undefined

-- assign a client a nick name
nickClient :: String -> Infra.Address -> StateT ServerStore IO ()
nickClient nick addr = undefined

-- multicast message to all clients in the given group
multiCastToClient :: Int -> Infra.Message -> StateT ServerStore IO ()
multiCastToClient group msg = undefined

-- multicast message to all servers (including itself)
multiCastToServer :: Infra.Message -> StateT ServerStore IO ()
multiCastToServer msg = undefined

-- how to handle invalid input?
-- use exceptT to handle invalid case?
parse :: String -> Infra.Message
parse input =
    case words input of
        "/join" : xs    -> Join 0
        "/text" : xs    -> Text $ unwords xs
        "/nick" : xs    -> Nick $ unwords xs
        "/part" : []    -> Part
        "/quit" : []    -> Quit
        _               -> Quit

toString :: Infra.Message -> String
toString = undefined

runServer :: IO ()
runServer = undefined

main :: IO ()
main = runServer
