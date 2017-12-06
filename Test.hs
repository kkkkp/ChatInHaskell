{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
#-}

module Test where

import Infra  as Infra
import Config as Config
import Server hiding (main)
import Test.QuickCheck
import Network.Socket
import qualified Data.Map as M
import Control.Monad.State

prop_join :: Socket -> Int -> Int -> Bool
prop_join sock n p = M.member addr map where
    port = Infra.intToPortNumber p
    addr = (SockAddrInet port Config.host)
    map  = getClient $ execState (assignClient n addr) $ ServerStore sock M.empty M.empty

prop_join_s :: Bool
prop_join_s = True

prop_join_f :: Bool
prop_join_f = True

prop_nick :: Socket -> String -> Int -> Bool
prop_nick sock name p = name == nickName where
    port     = Infra.intToPortNumber p
    addr     = (SockAddrInet port Config.host)
    map      = getClient $ execState (nickClient name addr) $ ServerStore sock M.empty M.empty
    nickName = case M.lookup addr map of
        Just client -> nick client
        Nothing -> "-ERR"

prop_nick_s :: Bool
prop_nick_s = True

prop_nick_f :: Bool
prop_nick_f = True

prop_send_s :: Bool
prop_send_s = True

prop_send_f :: Bool
prop_send_f = True

prop_part :: Socket -> Int -> Bool
prop_part sock p = not (M.member addr map) where
    port = Infra.intToPortNumber p
    addr = SockAddrInet port Config.host
    map  = getClient $ execState (partClient addr) $ ServerStore sock M.empty M.empty

prop_part_s :: Bool
prop_part_s = True

prop_part_f :: Bool
prop_part_f = True

prop_multicast_client :: Bool
prop_multicast_client = True

prop_multicast_server :: Bool
prop_multicast_server = True

test0 :: IO ()
test0 = do
    sock <- socket AF_INET Datagram 0
    quickCheck ((prop_join sock) :: Int -> Int -> Bool)

test1 :: IO ()
test1 = quickCheck (prop_join_s :: Bool)

test2 :: IO ()
test2 = quickCheck (prop_join_f :: Bool)

test3 :: IO ()
test3 = do
    sock <- socket AF_INET Datagram 0
    quickCheck ((prop_nick sock) :: String -> Int -> Bool)

test4 :: IO ()
test4 = quickCheck (prop_nick_s :: Bool)

test5 :: IO ()
test5 = quickCheck (prop_nick_f :: Bool)

test6 :: IO ()
test6 = quickCheck (prop_send_s :: Bool)

test7 :: IO ()
test7 = quickCheck (prop_send_f :: Bool)

test8 :: IO ()
test8 = do
    sock <- socket AF_INET Datagram 0
    quickCheck ((prop_part sock) :: Int -> Bool)

test9 :: IO ()
test9 = quickCheck (prop_part_s :: Bool)

test10 :: IO ()
test10 = quickCheck (prop_part_f :: Bool)

test11 :: IO ()
test11 = quickCheck (prop_multicast_client :: Bool)

test12 :: IO ()
test12 = quickCheck (prop_multicast_server :: Bool)

runTests :: IO ()
runTests = test0 >> test1 >> test2 >> test3 >> test4 >> test5 >> test6 >> test7 >> test8 >> test9 >> test10 >> test11 >> test12

main :: IO ()
main = do
    runTests
