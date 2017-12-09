{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
, MultiParamTypeClasses
#-}

{-# OPTIONS_GHC
    -w
#-}

module Server where

import Infra
import qualified Data.Map as M
import Control.Monad.State as S
import Network.Socket
import Config

-- instantiate an instance of monad socket
instance MonadSocket IO Socket where
  mySend sock mesg addr = do
    putStr $ "S -> <" ++ (show addr) ++ "> --- " ++ mesg
    _ <- sendTo sock mesg addr
    return ()

-- one iter of a transaction
runServer :: Socket -> ServerStore -> IO ()
runServer sock store = do
    (msg, recv_count, addr) <- recvFrom sock maxline
    let serverMap = getServer store
    let mesg = (unwords . lines) msg
    let client   = addr
    let nickName = getNick store client
    putStrLn $ "S <- <" ++ (show addr) ++ "> --- " ++ mesg
    case M.lookup addr serverMap of
        Just _  -> case parse mesg of
                    Serv n text -> do
                        (_, store') <- runStateT (servHandler sock addr n text) store
                        runServer sock store'
                    _           -> runServer sock store
        Nothing -> case parse mesg of
                    Join n      -> do
                        (_, store') <- runStateT (joinHandler sock n client) store
                        runServer sock store'
                    Nick name   -> do
                        (_, store') <- runStateT (nickHandler sock client name) store
                        runServer sock store'
                    Text text   -> do
                        (_, store') <- runStateT (textHandler sock client text nickName) store
                        runServer sock store'
                    Part        -> do
                        (_, store') <- runStateT (partHandler sock client) store
                        runServer sock store'
                    Quit        -> do
                        (_, store') <- runStateT (quitHandler sock client) store
                        runServer sock store'
                    Kill        -> return ()

-- main driver
main :: Int -> IO ()
main i = do
    let port = ports !! i
    sock <- socket AF_INET Datagram 0
    setSocketOption sock ReuseAddr 1
    setSocketOption sock ReusePort 1
    bindSocket sock (SockAddrInet port iNADDR_ANY)

    (_, store) <- runStateT (configServer ports 0) $ ServerStore M.empty M.empty

    putStrLn $ "[S" ++ (show port) ++ "] starting..."
    runServer sock store
    putStrLn $ "[S" ++ (show port) ++ "] closing..."

    return ()
