{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
#-}

module Config where

import Network.Socket

host    = tupleToHostAddress (127, 0, 0, 1)
maxline = 1500 :: Int
ports   = [40000, 40001] :: [PortNumber]
len     = length ports
