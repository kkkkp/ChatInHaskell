{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
#-}

{-# OPTIONS_GHC
    -w
#-}

module Test where

import Infra
import Config
import Test.QuickCheck
import Network.Socket
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad (liftM, liftM2, liftM3)

instance Arbitrary ClientStore where
    arbitrary = ClientStore <$> (arbitrary :: Gen String) <*> (arbitrary :: Gen Int)

instance Arbitrary SockAddr where
    arbitrary = flip SockAddrInet host <$> port where
        port = intToPortNumber <$> (arbitrary :: Gen Int)

instance Arbitrary ServerStore where
    arbitrary = ServerStore <$> clientInfo <*> serverInfo where
        clients    = listOf $ liftM2 (,) arbitrary arbitrary
        servers    = listOf $ liftM2 (,) arbitrary arbitrary
        clientInfo = foldr (\(addr, client) map -> M.insert addr client map) M.empty <$> clients
        serverInfo = foldr (\(addr, n) map -> M.insert addr n map) M.empty <$> servers

prop_join :: ServerStore -> Int -> SockAddr -> Bool
prop_join store n addr = M.member addr map where
    map  = getClient $ execState (assignClient n addr) $ store

prop_join_s :: Bool
prop_join_s = True

prop_join_f :: Bool
prop_join_f = True

prop_nick :: ServerStore -> String -> SockAddr -> Bool
prop_nick store name addr = name == nickName where
    map      = getClient $ execState (nickClient name addr) $ store
    nickName = case M.lookup addr map of
        Just client -> nick client
        Nothing -> name ++ "-ERR"

prop_nick_s :: Bool
prop_nick_s = True

prop_nick_f :: Bool
prop_nick_f = True

prop_send_s :: Bool
prop_send_s = True

prop_send_f :: Bool
prop_send_f = True

prop_part :: ServerStore -> SockAddr -> Bool
prop_part store addr = not (M.member addr map) where
    map  = getClient $ execState (partClient addr) $ store

prop_part_s :: Bool
prop_part_s = True

prop_part_f :: Bool
prop_part_f = True

prop_multicast_client :: Bool
prop_multicast_client = True

prop_multicast_server :: Bool
prop_multicast_server = True

test0 :: IO ()
test0 = quickCheck (prop_join :: ServerStore -> Int -> SockAddr -> Bool)

test1 :: IO ()
test1 = quickCheck (prop_join_s :: Bool)

test2 :: IO ()
test2 = quickCheck (prop_join_f :: Bool)

test3 :: IO ()
test3 = quickCheck (prop_nick :: ServerStore -> String -> SockAddr -> Bool)

test4 :: IO ()
test4 = quickCheck (prop_nick_s :: Bool)

test5 :: IO ()
test5 = quickCheck (prop_nick_f :: Bool)

test6 :: IO ()
test6 = quickCheck (prop_send_s :: Bool)

test7 :: IO ()
test7 = quickCheck (prop_send_f :: Bool)

test8 :: IO ()
test8 = quickCheck (prop_part :: ServerStore -> SockAddr -> Bool)

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
