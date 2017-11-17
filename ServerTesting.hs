{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
#-}

module ServerTesting where

import qualified Data.Map as M
import Control.Monad.State as S

data Message = JoinRoom Int
             | TextData String
             | Disconnect

newtype AbstractSocket =
  AbstractSocket { getAbstractSocket :: Int }

class MonadSocket m where
  readFrom :: AbstractSocket -> m Message
  sendTo :: AbstractSocket -> Message -> m ()

data ServerState = ServerState {
  connectedUsers :: M.Map AbstractSocket Int
  }

serverIter :: (MonadSocket m, MonadState ServerState m)
           => AbstractSocket
           -> m ()
serverIter sock = do
  msg <- readFrom sock
  case msg of
    JoinRoom roomNo ->
      {- Remove user from any current room, and move he/she into the new room -}
      {- Send back a text data message to confirm the room change -}
      return ()
    Disconnect ->
      {- Remove the user from the connected user map -}
      return ()
