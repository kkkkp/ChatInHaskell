{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
, MultiParamTypeClasses
#-}

module Infra where

import Network.Socket
import qualified Data.Map as M

data Message = Join Int
             | Text String
             | Serv Int String
             | Nick String
             | Part
             | Quit
             | Kill
    deriving (Show, Eq)

class Monad m => MonadSocket m s where
  mySend :: s -> String -> SockAddr -> m ()

instance MonadSocket IO Socket where
  mySend sock mesg addr = do
    putStr $ "S -> <" ++ (show addr) ++ "> --- " ++ mesg
    _ <- sendTo sock mesg addr
    return ()

intToPortNumber :: Int -> PortNumber
intToPortNumber n = if n >= 0 then read (show n) :: PortNumber else read "5000" :: PortNumber

send :: Socket -> String -> SockAddr -> IO ()
send sock mesg addr = do
  putStr $ "S -> <" ++ (show addr) ++ "> --- " ++ mesg
  _ <- sendTo sock mesg addr
  return ()
