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
            count <- sendTo sock ((nick client) ++ ": " ++ msg) addr
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
            count <- sendTo sock ((show n) ++ "$ " ++ msg) addr
            return ()

-- TODO: how to handle invalid input?
-- implement a type class of parser?
-- use exceptT to handle invalid case?
-- parse :: (MonadError String m) => String -> m Infra.Message
parse :: String -> Infra.Message
parse input =
    case words input of
        "/join" : xs    -> Join 0
        "/text" : xs    -> STxt 0 $ unwords xs
        "/nick" : xs    -> Nick $ unwords xs
        "/part" : []    -> Part
        "/quit" : []    -> Quit
        other           -> Text $ unwords other

toString :: Infra.Message -> String
toString (Text str) = str
toString _          = "-ERR Not supported"

-- should implement some kind of loop
runServer :: ServerStore -> IO ()
runServer store = do
    let sock = getSock store
    (msg, recv_count, client) <- recvFrom sock maxline
    let mesg = (unwords . lines) msg
    putStrLn ("S: " ++ mesg)
    case parse mesg of
        Quit        -> do
            putStrLn "S: closing..."
        Part        -> do
            putStrLn "S: parting..."
            runServer store
        Join n      -> do
            putStrLn "S: joining..."
            runServer store
        Nick name   -> do
            putStrLn "S: nicking..."
            runServer store
        STxt n text -> do
            putStrLn "S: texting..."
            runServer store
        Text text   -> do
            putStrLn $ "S: recving... " ++ text
            runServer store


main :: IO ()
main = do
    sock <- socket AF_INET Datagram 0
    setSocketOption sock ReuseAddr 1
    setSocketOption sock ReusePort 1
    bindSocket sock (SockAddrInet port iNADDR_ANY)
    let store = ServerStore sock M.empty M.empty
    putStrLn "Server starting..."
    runServer store
    putStrLn "Server closing..."
    return ()
