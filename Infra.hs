{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
#-}

module Infra where

import Network.Socket as NS

data Message = Join Int
             | Text String
             | Serv Int String
             | Nick String
             | Part
             | Quit
             | Kill
    deriving (Show, Eq)

-- class MonadSocket m s addr where
--   send :: s -> String -> addr -> m ()
--
-- instance MonadSocket IO Socket SockAddr where

intToPortNumber :: Int -> PortNumber
intToPortNumber n = if n >= 0 then read (show n) :: PortNumber else read "5000" :: PortNumber

send :: Socket -> String -> SockAddr -> IO ()
send sock mesg addr = do
    putStr $ "S -> <" ++ (show addr) ++ "> --- " ++ mesg
    _ <- sendTo sock mesg addr
    return ()
