{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
#-}

module Infra where

import Network.Socket as NS

data Message = Join Int
             | Text String
             | STxt Int String
             | Nick String
             | Part
             | Quit
             | Kill
    deriving (Show, Eq)
