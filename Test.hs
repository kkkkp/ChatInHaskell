{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
, MultiParamTypeClasses
, TypeSynonymInstances
, FlexibleInstances
#-}

-- {-# OPTIONS_GHC
--     -w
-- #-}

module Test where

import Infra
import Config
import Test.QuickCheck
import Network.Socket
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad (liftM, liftM2, liftM3)
import Data.List

data TestStore = TestStore {
    getMesgMap :: M.Map SockAddr String
}

type TestMonad = State TestStore

-- TODO: which monad should I use for test?
-- State
-- TODO: can I change the () -> a? What should I return if change to a?
instance MonadSocket TestMonad Int where
    mySend n mesg addr = do
        store <- get
        let map = getMesgMap store
        put $ store {getMesgMap = M.insert addr mesg map}

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

-- test join will add the sockaddr to the store
prop_join :: ServerStore -> Int -> SockAddr -> Bool
prop_join store n addr = M.member addr map where
    map  = getClient $ execState (assignClient n addr) $ store

f :: ServerStore -> TestStore
f = undefined

-- test "+OK" response for join
prop_join_s :: Int -> Int -> Int -> SockAddr -> Bool
prop_join_s sock roomID n addr = flag where
    map = getMesgMap $ execState (execStateT (joinHandler sock roomID n addr) (ServerStore M.empty M.empty)) (TestStore M.empty)
    mesg = case M.lookup addr map of
        Just m  -> m
        Nothing -> "#ERR"
    flag = if roomID == -1 then isPrefixOf "+OK" mesg else isPrefixOf "-ERR" mesg

-- test "-ERR" response for join
prop_join_f :: Bool
prop_join_f = True

-- test nick will change the name in the store
prop_nick :: ServerStore -> String -> SockAddr -> Bool
prop_nick store name addr = name == nickName where
    map      = getClient $ execState (nickClient name addr) $ store
    nickName = case M.lookup addr map of
        Just client -> nick client
        Nothing -> name ++ "-ERR"

-- test "+OK" response for nick
prop_nick_s :: Bool
prop_nick_s = True

-- test "-ERR" response for nick
prop_nick_f :: Bool
prop_nick_f = True

-- test "+OK" response for text
prop_text_s :: Bool
prop_text_s = True

-- test "-ERR" response for text
prop_text_f :: Bool
prop_text_f = True

-- test part will remove the sockaddr from the store
prop_part :: ServerStore -> SockAddr -> Bool
prop_part store addr = not (M.member addr map) where
    map  = getClient $ execState (partClient addr) $ store

-- test "+OK" response for part
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
test1 = quickCheck (prop_join_s :: Int -> Int -> Int -> SockAddr -> Bool)

test2 :: IO ()
test2 = quickCheck (prop_join_f :: Bool)

test3 :: IO ()
test3 = quickCheck (prop_nick :: ServerStore -> String -> SockAddr -> Bool)

test4 :: IO ()
test4 = quickCheck (prop_nick_s :: Bool)

test5 :: IO ()
test5 = quickCheck (prop_nick_f :: Bool)

test6 :: IO ()
test6 = quickCheck (prop_text_s :: Bool)

test7 :: IO ()
test7 = quickCheck (prop_text_f :: Bool)

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
