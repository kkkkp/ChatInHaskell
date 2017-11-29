{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
#-}

module Server where

import Infra as Infra
import qualified Data.Map as M
import Control.Monad.State as S
import Network.Socket

data ClientStore = ClientStore {nick :: String, room :: Int}

data ServerStore = ServerStore {sock :: Socket, getClient :: M.Map SockAddr ClientStore, getServer :: M.Map Int SockAddr}


-- TODO: how to use the State in the monad?
-- register a server with id and addr
registerServer :: Int -> SockAddr -> StateT ServerStore IO ()
registerServer n addr = do
    store <- get
    let serverAddr = getServer store
    put $ store {getServer = M.insert n addr serverAddr}
    return ()

-- register a client with addr to grp 0 (defualt)
registerClient :: SockAddr -> StateT ServerStore IO ()
registerClient = assignClient 0

-- assign a client to a different group
assignClient :: Int -> SockAddr -> StateT ServerStore IO ()
assignClient n addr = do
    store <- get
    let client = ClientStore (show addr) n
    let map    = getClient store
    put $ store {getClient = M.insert addr client map}
    return ()

-- disconnect a client from the group
partClient :: SockAddr -> StateT ServerStore IO ()
partClient addr = do
    store <- get
    let map = getClient store
    put $ store {getClient = M.delete addr map}
    return ()

nickClientHelper :: String -> SockAddr -> M.Map SockAddr ClientStore -> ClientStore
nickClientHelper name addr map = case M.lookup addr map of
    Just c  -> c {nick = name}
    Nothing -> ClientStore name 0

-- assign a client a nick name
nickClient :: String -> SockAddr -> StateT ServerStore IO ()
nickClient name addr = do
    store <- get
    let map    = getClient store
    let client = nickClientHelper name addr map
    put $ store {getClient = M.insert addr client map}
    return ()

errorClient :: SockAddr -> StateT ServerStore IO ()
errorClient addr = do
    return ()

-- multicast message to all clients in the given group
multiCastToClient :: Int -> String -> StateT ServerStore IO ()
multiCastToClient n msg = do
    store <- get
    return ()

-- multicast message to all servers (including itself)
multiCastToServer :: String -> StateT ServerStore IO ()
multiCastToServer msg = do
    return ()

-- TODO: how to handle invalid input?
-- implement a type class of parser?
-- use exceptT to handle invalid case?
-- parse :: (MonadError String m) => String -> m Infra.Message
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
toString (Text str) = str
toString _          = "-ERR Not supported"

-- should implement some kind of loop
runServer :: IO ()
runServer = do
    return ()

main :: IO ()
main = do
    return ()
