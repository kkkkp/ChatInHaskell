{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
#-}

module Server where

import Infra as Infra hiding (ServerStore, ClientStore)
import qualified Data.Map as M
import Control.Monad.State as S
import Network.Socket
import Config as Config

data ClientStore = ClientStore {nick :: String, room :: Int} deriving (Show, Eq)

data ServerStore = ServerStore {getSock :: Socket, getClient :: M.Map SockAddr ClientStore, getServer :: M.Map SockAddr Int} deriving (Show, Eq)

-- parse string to Message
parse :: String -> Infra.Message
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
    registerServer n (SockAddrInet port Config.host)
    configServer ports (n + 1)

-- register a client with addr to grp 0 (defualt)
registerClient :: (MonadState ServerStore m) => SockAddr -> m ()
registerClient = assignClient 0

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
multiCastToClient :: Int -> String -> StateT ServerStore IO ()
multiCastToClient n msg = do
    store <- get
    let map  = getClient store
    let sock = getSock store
    _ <- lift $ M.traverseWithKey (send sock msg) map
    return ()
    where
        send :: Socket -> String -> SockAddr -> ClientStore -> IO ()
        send sock msg addr client = do
            if room client == n
            then do
                Infra.send sock (msg ++ "\n") addr
                return ()
            else do
                return ()

-- multicast message to all servers (including itself)
multiCastToServer :: Int -> String -> StateT ServerStore IO ()
multiCastToServer n msg = do
    store <- get
    let map  = getServer store
    let sock = getSock store
    _ <- lift $ M.traverseWithKey (send sock store n msg) map
    return ()
    where
        send :: Socket -> ServerStore -> Int -> String -> SockAddr -> Int -> IO ()
        send sock store n msg addr _ = do
            Infra.send sock (format n msg ++ "\n") addr
            return ()

-- join handler
joinHandler :: (MonadSocket m s) => s -> Int -> Int -> SockAddr -> StateT ServerStore m ()
joinHandler sock roomID n client = do
  if roomID == -1
  then do
      assignClient n client
      lift $ mySend sock ("+OK you join to #" ++ (show n) ++ "\n") client
      return ()
  else do
      lift $ mySend sock ("-ERR you are in #" ++ (show roomID) ++ "\n") client
      return ()

-- nick handler
nickHandler :: (MonadSocket m s) => s -> Int-> SockAddr -> String -> StateT ServerStore m ()
nickHandler sock roomID client name = do
    if roomID == -1
    then do
        lift $ mySend sock ("-ERR you are not in any room\n") client
        return ()
    else do
        nickClient name client
        lift $ mySend sock ("+OK you nick to <" ++ name ++ ">\n") client
        return ()

-- text handler
textHandler :: Socket -> Int -> ServerStore -> SockAddr -> String -> String -> IO ()
textHandler sock roomID store client text name = do
    if roomID == -1
    then do
        Infra.send sock ("-ERR you are not in any room\n") client
        runServer store
    else do
        _ <- runStateT (multiCastToServer roomID ("<" ++ name ++ "> " ++ text)) $ store
        runServer store

-- part handler
partHandler :: (MonadSocket m s) => s -> Int -> SockAddr -> StateT ServerStore m ()
partHandler sock roomID client = do
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
servHandler :: Socket -> ServerStore -> SockAddr -> Int -> String -> IO ()
servHandler sock store addr n text = do
    _ <- runStateT (multiCastToClient n text) $ store
    runServer store

-- one iter of a running
runServer :: ServerStore -> IO ()
runServer store = do
    let sock      = getSock store
    (msg, recv_count, addr) <- recvFrom sock Config.maxline
    let serverMap = getServer store
    let mesg = (unwords . lines) msg
    let client   = addr
    let roomID   = getRoom store client
    let nickName = getNick store client
    putStrLn $ "S <- <" ++ (show addr) ++ "> --- " ++ mesg
    case M.lookup addr serverMap of
        Just _  -> case parse mesg of
                    Serv n text -> servHandler sock store addr n text
                    _           -> runServer store
        Nothing -> case parse mesg of
                    Join n      -> do
                        (_, store') <- runStateT (joinHandler sock roomID n client) store
                        runServer store'
                    Nick name   -> do
                        (_, store') <- runStateT (nickHandler sock roomID client name) store
                        runServer store'
                    Text text   -> textHandler sock roomID store client text nickName
                    Part        -> do
                        (_, store') <- runStateT (partHandler sock roomID client) store
                        runServer store'
                    Quit        -> do
                        (_, store') <- runStateT (quitHandler sock client) store
                        runServer store'
                    Kill        -> return ()

-- main driver
main :: Int -> IO ()
main i = do
    let port = Config.ports !! i
    sock <- socket AF_INET Datagram 0
    setSocketOption sock ReuseAddr 1
    setSocketOption sock ReusePort 1
    bindSocket sock (SockAddrInet port iNADDR_ANY)
    let store = ServerStore sock M.empty M.empty
    (_, store') <- runStateT (configServer Config.ports 0) $ store
    putStrLn $ "[S" ++ (show port) ++ "] starting..."
    runServer store'
    putStrLn $ "[S" ++ (show port) ++ "] closing..."

    return ()
