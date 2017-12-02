{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
#-}

module Infra where

import Network.Socket as NS

host    = NS.tupleToHostAddress (127, 0, 0, 1)
maxline = 1500 :: Int
ports    = [40000, 40001] :: [NS.PortNumber]

data Message = Join Int
             | Text String
             | Serv Int String
             | Nick String
             | Part
             | Quit
             | Kill
    deriving (Show, Eq)

send :: Socket -> String -> SockAddr -> IO ()
send sock mesg addr = do
    putStr $ "S -> <" ++ (show addr) ++ "> --- " ++ mesg
    _ <- sendTo sock mesg addr
    return ()
