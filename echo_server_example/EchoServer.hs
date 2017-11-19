module Main (main) where

import Network.Socket
import System.Posix.Directory
import System.Posix.Files
import System.Posix.IO
import System.Posix.Process
import System.Exit

echoPort = 4000
maxline = 1500

main :: IO ()
main = do
           echoserver
           exitImmediately ExitSuccess

-- how to set useability?
echoserver :: IO ()
echoserver = do
          withSocketsDo $ do
              sock <- socket AF_INET Datagram 0
              setSocketOption sock ReuseAddr 1
              setSocketOption sock ReusePort 1
              bindSocket sock (SockAddrInet echoPort iNADDR_ANY)
              socketEcho sock


socketEcho :: Socket -> IO ()
socketEcho sock = do
          (mesg, recv_count, client) <- recvFrom sock maxline
          case mesg of
            "/kill\n" -> do
                      putStrLn "Server closed!"
            _       -> do
                      putStr $ "S: " ++ mesg
                      send_count <- sendTo sock ((show client) ++ ": " ++ mesg) client
                      socketEcho sock
