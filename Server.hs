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

data Chatroom = Chatroom {getUser :: M.Map SockAddr ClientStore}

data ServerStore = ServerStore {getGroup :: M.Map Int Chatroom, getServer :: M.Map Int SockAddr}


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

-- assign a client to a different group helper
assignClientHelper :: Int -> SockAddr -> M.Map Int Chatroom -> M.Map Int Chatroom
assignClientHelper n addr groups = case M.lookup n groups of
    Just room -> M.insert n (Chatroom (M.insert addr (ClientStore (show addr) n) (getUser room))) groups
    Nothing   -> M.insert n (Chatroom (M.insert addr (ClientStore (show addr) n) M.empty)) groups

-- assign a client to a different group
assignClient :: Int -> SockAddr -> StateT ServerStore IO ()
assignClient n addr = do
    store <- get
    let groups = getGroup store
    put $ store {getGroup = assignClientHelper n addr groups}
    return ()

-- disconnect a client from the group
partClient :: Int -> SockAddr -> StateT ServerStore IO ()
partClient group addr = do
    return ()

-- assign a client a nick name
nickClient :: String -> SockAddr -> StateT ServerStore IO ()
nickClient nick addr = undefined

errorClient :: SockAddr -> StateT ServerStore IO ()
errorClient addr = undefined

deliverMsgToGroup :: Infra.Message -> StateT ServerStore IO ()
deliverMsgToGroup msg = undefined

-- multicast message to all clients in the given group
multiCastToClient :: Int -> Infra.Message -> StateT ServerStore IO ()
multiCastToClient group msg = undefined

-- multicast message to all servers (including itself)
multiCastToServer :: Infra.Message -> StateT ServerStore IO ()
multiCastToServer msg = undefined

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
runServer = undefined

main :: IO ()
main = runServer
