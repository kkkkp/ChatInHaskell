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

data ServerStore = ServerStore {getSock :: Socket, getClient :: M.Map SockAddr ClientStore, getServer :: M.Map Int SockAddr}

port    = 4000
maxline = 1500

-- register a server with id and addr
registerServer :: Int -> SockAddr -> StateT ServerStore IO ()
registerServer n addr = do
    store <- get
    let serverAddr = getServer store
    put $ store {getServer = M.insert n addr serverAddr}

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

errorClient :: SockAddr -> StateT ServerStore IO ()
errorClient addr = do
    return ()

-- multicast message to all clients in the given group
-- TODO: check size of sent mesg
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
            count <- sendTo sock (msg ++ "\n") addr
            return ()

-- multicast message to all servers (including itself)
multiCastToServer :: Int -> String -> StateT ServerStore IO ()
multiCastToServer n msg = do
    store <- get
    let map  = getServer store
    let sock = getSock store
    _ <- lift $ M.traverseWithKey (send sock n msg) map
    return ()
    where
        send :: Socket -> Int -> String -> Int -> SockAddr -> IO ()
        send sock n msg _ addr = do
            count <- sendTo sock msg addr
            return ()

-- TODO: how to handle invalid input?
-- implement a type class of parser?
-- use exceptT to handle invalid case?
-- parse :: (MonadError String m) => String -> m Infra.Message
parse :: String -> Infra.Message
parse input =
    case words input of
        "/join" : xs    -> Join 0
        "$text" : xs    -> STxt 0 $ unwords xs
        "/nick" : xs    -> Nick $ unwords xs
        "/part" : []    -> Part
        "/quit" : []    -> Quit
        "$kill" : []    -> Kill
        other           -> Text $ unwords other

getRoom :: ServerStore -> SockAddr -> Int
getRoom store addr = case M.lookup addr (getClient store) of
    Just client -> room client
    Nothing     -> -1

format :: String -> ServerStore -> SockAddr -> String
format msg store addr = case M.lookup addr (getClient store) of
    Just client -> "$text " ++ (nick client) ++ ": " ++ msg ++ "\n"
    Nothing     -> "$text " ++ (show addr) ++ ": " ++ msg ++ "\n"

-- should implement some kind of loop
runServer :: ServerStore -> IO ()
runServer store = do
    let sock = getSock store
    (msg, recv_count, client) <- recvFrom sock maxline
    let mesg = (unwords . lines) msg
    let roomID    = getRoom store client
    putStrLn ("S: " ++ mesg)
    case parse mesg of
        Kill        -> do
            putStrLn "S: closing..."
        Quit        -> do
            putStrLn "S: quitting..."
            _ <- sendTo sock ("+OK you are disconnected\n") client
            runServer store
        Part        -> do
            putStrLn "S: parting..."
            (_, store') <- runStateT (partClient client) $ store
            _ <- sendTo sock ("+OK you part from #" ++ (show roomID) ++ " and join #0\n") client
            runServer store'
        Join n      -> do
            putStrLn "S: joining..."
            (_, store') <- runStateT (registerClient client) $ store
            _ <- sendTo sock ("+OK you join to #" ++ (show n) ++ "\n") client
            runServer store'
        Nick name   -> do
            putStrLn "S: nicking..."
            (_, store') <- runStateT (nickClient name client) $ store
            _ <- sendTo sock ("+OK you nick to <" ++ name ++ ">\n") client
            runServer store'
        STxt n text -> do
            putStrLn "S: texting..."
            _ <- runStateT (multiCastToClient n text) $ store
            runServer store
        Text text   -> do
            putStrLn $ "S: recving... " ++ text
            if roomID == -1
            then do
                _ <- sendTo sock ("-ERR you are not in any room\n") client
                runServer store
            else do
                _ <- runStateT (multiCastToServer roomID (format text store client)) $ store
                runServer store


main :: IO ()
main = do
    sock <- socket AF_INET Datagram 0
    setSocketOption sock ReuseAddr 1
    setSocketOption sock ReusePort 1
    bindSocket sock (SockAddrInet port iNADDR_ANY)
    let store = ServerStore sock M.empty M.empty
    (_, store') <- runStateT (registerServer 0 (SockAddrInet port iNADDR_ANY)) $ store
    putStrLn "[Server " ++ (show port) ++ "] starting..."
    runServer store'
    putStrLn "[Server " ++ (show port) ++ "] closing..."
    return ()
