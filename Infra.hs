{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
, MultiParamTypeClasses
#-}

{-# OPTIONS_GHC
    -w
#-}

module Infra where

import qualified Data.Map as M
import Control.Monad.State as S
import Network.Socket
import Config

-- Message
data Message = Join Int
             | Text String
             | Serv Int String
             | Nick String
             | Part
             | Quit
             | Kill
    deriving (Show, Eq)

-- Client information store
data ClientStore = ClientStore {
    nick :: String,
    room :: Int
} deriving (Show, Eq)

-- Server information store
data ServerStore = ServerStore {
    getClient :: M.Map SockAddr ClientStore,
    getServer :: M.Map SockAddr Int
} deriving (Show, Eq)

-- MonadSocket class
class Monad m => MonadSocket m s where
  mySend :: s -> String -> SockAddr -> m ()

-- parse string to Message
parse :: String -> Message
parse input =
    case words input of
        "/join" : xs       -> Join (read (unwords xs) :: Int)
        "$serv" : num : xs -> Serv (read num :: Int) (unwords xs)
        "/nick" : xs       -> Nick $ unwords xs
        "/part" : []       -> Part
        "/quit" : []       -> Quit
        "$kill" : []       -> Kill
        other              -> Text $ unwords other

format :: Int -> String -> String
format n msg = "$serv " ++ (show n) ++ " " ++ msg

-- get roomID
getRoom :: ServerStore -> SockAddr -> Int
getRoom store addr = case M.lookup addr (getClient store) of
    Just client -> room client
    Nothing     -> -1

-- get nick name
getNick :: ServerStore -> SockAddr -> String
getNick store addr = case M.lookup addr (getClient store) of
    Just client -> nick client
    Nothing     -> show addr

-- transform int to portnumber
intToPortNumber :: Int -> PortNumber
intToPortNumber n = if n >= 0 then read (show n) :: PortNumber else read "5000" :: PortNumber

-- register a server with id and addr
registerServer :: (MonadState ServerStore m) => Int -> SockAddr -> m ()
registerServer n addr = do
    store <- get
    let serverAddr = getServer store
    put $ store {getServer = M.insert addr n serverAddr}

-- config servers
configServer :: (MonadState ServerStore m) => [PortNumber] -> Int -> m ()
configServer [] _ = do
    store <- get
    put $ store
configServer (port : ports) n = do
    registerServer n (SockAddrInet port host)
    configServer ports (n + 1)

-- assign a client to a different group
assignClient :: (MonadState ServerStore m) => Int -> SockAddr -> m ()
assignClient n addr = do
    store <- get
    let client = ClientStore (show addr) n
    let map    = getClient store
    put $ store {getClient = M.insert addr client map}

-- disconnect a client from the group
partClient :: (MonadState ServerStore m) => SockAddr -> m ()
partClient addr = do
    store <- get
    let map = getClient store
    put $ store {getClient = M.delete addr map}

nickClientHelper :: String -> SockAddr -> M.Map SockAddr ClientStore -> ClientStore
nickClientHelper name addr map = case M.lookup addr map of
    Just c  -> c {nick = name}
    Nothing -> ClientStore name 0

-- assign a client a nick name
nickClient :: (MonadState ServerStore m) => String -> SockAddr -> m ()
nickClient name addr = do
    store <- get
    let map    = getClient store
    let client = nickClientHelper name addr map
    put $ store {getClient = M.insert addr client map}

-- multicast message to all clients in the given group
multiCastToClient :: (MonadSocket m s) => s -> Int -> String -> StateT ServerStore m ()
multiCastToClient sock n msg = do
    store <- get
    let map  = getClient store
    lift $ M.traverseWithKey (send sock msg) map
    return ()
    where
        send sock msg addr client = do
            if room client == n
            then do
                mySend sock (msg ++ "\n") addr
                return ()
            else do
                return ()

-- multicast message to all servers (including itself)
multiCastToServer :: (MonadSocket m s) => s -> Int -> String -> StateT ServerStore m ()
multiCastToServer sock n msg = do
    store <- get
    let map  = getServer store
    lift $ M.traverseWithKey (send sock n msg) map
    return ()
    where
        send sock n msg addr _ = do
            mySend sock (format n msg ++ "\n") addr
            return ()

-- join handler
joinHandler :: (MonadSocket m s) => s -> Int -> SockAddr -> StateT ServerStore m ()
joinHandler sock n client = do
  store <- get
  let roomID = getRoom store client
  if roomID == -1
  then do
      assignClient n client
      lift $ mySend sock ("+OK you join to #" ++ (show n) ++ "\n") client
      return ()
  else do
      lift $ mySend sock ("-ERR you are in #" ++ (show roomID) ++ "\n") client
      return ()

-- nick handler
nickHandler :: (MonadSocket m s) => s -> SockAddr -> String -> StateT ServerStore m ()
nickHandler sock client name = do
    store <- get
    let roomID = getRoom store client
    if roomID == -1
    then do
        lift $ mySend sock ("-ERR you are not in any room\n") client
        return ()
    else do
        nickClient name client
        lift $ mySend sock ("+OK you nick to <" ++ name ++ ">\n") client
        return ()

-- text handler
textHandler :: (MonadSocket m s) => s -> SockAddr -> String -> String -> StateT ServerStore m ()
textHandler sock client text name = do
    store <- get
    let roomID = getRoom store client
    if roomID == -1
    then do
        lift $ mySend sock ("-ERR you are not in any room\n") client
    else do
        multiCastToServer sock roomID ("<" ++ name ++ "> " ++ text)

-- part handler
partHandler :: (MonadSocket m s) => s -> SockAddr -> StateT ServerStore m ()
partHandler sock client = do
    store <- get
    let roomID = getRoom store client
    if roomID == -1
    then do
        lift $ mySend sock ("-ERR you are not in any room\n") client
        return ()
    else do
        partClient client
        lift $ mySend sock ("+OK you leave #" ++ (show roomID) ++ "\n") client
        return ()

-- quit handler
quitHandler :: (MonadSocket m s) => s -> SockAddr -> StateT ServerStore m ()
quitHandler sock client = do
    partClient client
    lift $ mySend sock ("+OK you are disconnected\n") client
    return ()

-- serv handler
servHandler :: (MonadSocket m s) => s -> SockAddr -> Int -> String -> StateT ServerStore m ()
servHandler sock _ = multiCastToClient sock
