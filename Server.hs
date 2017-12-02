{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
#-}

module Server where

import Infra as Infra
import qualified Data.Map as M
import Control.Monad.State as S
import Network.Socket

data ClientStore = ClientStore {nick :: String, room :: Int} deriving (Show, Eq)

data ServerStore = ServerStore {getSock :: Socket, getClient :: M.Map SockAddr ClientStore, getServer :: M.Map SockAddr Int} deriving (Show, Eq)

port    = 4000
host    = tupleToHostAddress (127, 0, 0, 1)
maxline = 1500

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
registerServer :: Int -> SockAddr -> StateT ServerStore IO ()
registerServer n addr = do
    store <- get
    let serverAddr = getServer store
    put $ store {getServer = M.insert addr n serverAddr}

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

-- disconnect a client from the group
partClient :: SockAddr -> StateT ServerStore IO ()
partClient addr = do
    store <- get
    let map = getClient store
    put $ store {getClient = M.delete addr map}

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
                count <- sendTo sock (msg ++ "\n") addr
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
            count <- sendTo sock (format n msg) addr
            return ()

-- join handler
joinHandler :: Socket -> Int -> ServerStore -> Int -> SockAddr -> IO ()
joinHandler sock roomID store n client = do
    if roomID == -1
    then do
        (_, store') <- runStateT (assignClient n client) $ store
        _ <- sendTo sock ("+OK you join to #" ++ (show n) ++ "\n") client
        runServer store'
    else do
        _ <- sendTo sock ("-ERR you are in #" ++ (show roomID) ++ "\n") client
        runServer store

-- nick handler
nickHandler :: Socket -> Int -> ServerStore -> SockAddr -> String -> IO ()
nickHandler sock roomID store client name = do
    if roomID == -1
    then do
        _ <- sendTo sock ("-ERR you are not in any room\n") client
        runServer store
    else do
        (_, store') <- runStateT (nickClient name client) $ store
        _ <- sendTo sock ("+OK you nick to <" ++ name ++ ">\n") client
        runServer store'

-- text handler
textHandler :: Socket -> Int -> ServerStore -> SockAddr -> String -> String -> IO ()
textHandler sock roomID store client text name = do
    if roomID == -1
    then do
        _ <- sendTo sock ("-ERR you are not in any room\n") client
        runServer store
    else do
        _ <- runStateT (multiCastToServer roomID (name ++ ": " ++ text)) $ store
        runServer store

-- part handler
partHandler :: Socket -> Int -> ServerStore -> SockAddr -> IO ()
partHandler sock roomID store client = do
    if roomID == -1
    then do
        _ <- sendTo sock ("-ERR you are not in any room\n") client
        runServer store
    else do
        (_, store') <- runStateT (partClient client) $ store
        _ <- sendTo sock ("+OK you leave #" ++ (show roomID) ++ "\n") client
        runServer store'

-- quit handler
quitHandler :: Socket -> ServerStore -> SockAddr -> IO ()
quitHandler sock store client = do
    (_, store') <- runStateT (partClient client) $ store
    _ <- sendTo sock ("+OK you are disconnected\n") client
    runServer store'

-- serv handler
servHandler :: Socket -> ServerStore -> SockAddr -> Int -> String -> IO ()
servHandler sock store addr n text = do
    _ <- runStateT (multiCastToClient n text) $ store
    runServer store

-- one iter of a running
runServer :: ServerStore -> IO ()
runServer store = do
    let sock      = getSock store
    (msg, recv_count, addr) <- recvFrom sock maxline
    let serverMap = getServer store
    let mesg = (unwords . lines) msg
    let client   = addr
    let roomID   = getRoom store client
    let nickName = getNick store client

    case M.lookup addr serverMap of
        Just _  -> case parse mesg of
                    Serv n text -> servHandler sock store addr n text
                    _           -> runServer store
        Nothing -> case parse mesg of
                    Join n      -> joinHandler sock roomID store n client
                    Nick name   -> nickHandler sock roomID store client name
                    Text text   -> textHandler sock roomID store client text nickName
                    Part        -> partHandler sock roomID store client
                    Quit        -> quitHandler sock store client
                    Kill        -> return ()

-- driver
main :: IO ()
main = do
    sock <- socket AF_INET Datagram 0
    setSocketOption sock ReuseAddr 1
    setSocketOption sock ReusePort 1
    bindSocket sock (SockAddrInet port iNADDR_ANY)
    let store = ServerStore sock M.empty M.empty
    (_, store') <- runStateT (registerServer 0 (SockAddrInet port host)) $ store

    putStrLn $ "[Server " ++ (show port) ++ "] starting..."
    runServer store'
    putStrLn $ "[Server " ++ (show port) ++ "] closing..."
    
    return ()
