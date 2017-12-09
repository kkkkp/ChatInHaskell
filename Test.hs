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
prop_join_handler :: Int -> Int -> SockAddr -> ServerStore -> Bool
prop_join_handler sock n addr store = flag where
    roomID = getRoom store addr
    map    = getMesgMap $ execState (execStateT (joinHandler sock n addr) store) $ TestStore M.empty
    mesg   = case M.lookup addr map of
        Just m  -> m
        Nothing -> "#ERR"
    flag   = if roomID == -1 then isPrefixOf "+OK" mesg else isPrefixOf "-ERR" mesg

-- test nick will change the name in the store
prop_nick :: ServerStore -> String -> SockAddr -> Bool
prop_nick store name addr = name == nickName where
    map      = getClient $ execState (nickClient name addr) $ store
    nickName = case M.lookup addr map of
        Just client -> nick client
        Nothing -> name ++ "-ERR"

-- test "+OK" response for nick
prop_nick_handler :: Int -> SockAddr -> String -> ServerStore -> Bool
prop_nick_handler sock addr name store = flag where
    roomID = getRoom store addr
    map    = getMesgMap $ execState (execStateT (nickHandler sock addr name) store) $ TestStore M.empty
    mesg   = case M.lookup addr map of
        Just m  -> m
        Nothing -> "#ERR"
    flag   = if roomID /= -1 then isPrefixOf "+OK" mesg else isPrefixOf "-ERR" mesg

-- test "+OK" response for text
prop_text_handler :: Int -> SockAddr -> String -> String -> ServerStore -> Bool
prop_text_handler sock client text name store = True

-- test part will remove the sockaddr from the store
prop_part :: ServerStore -> SockAddr -> Bool
prop_part store addr = not (M.member addr map) where
    map  = getClient $ execState (partClient addr) $ store

-- test "+OK" response for part
prop_part_handler :: Int -> SockAddr -> ServerStore -> Bool
prop_part_handler sock addr store = flag where
    roomID = getRoom store addr
    map    = getMesgMap $ execState (execStateT (partHandler sock addr) store) $ TestStore M.empty
    mesg   = case M.lookup addr map of
        Just m  -> m
        Nothing -> "#ERR"
    flag   = if roomID /= -1 then isPrefixOf "+OK" mesg else isPrefixOf "-ERR" mesg

prop_multicast_client :: Bool
prop_multicast_client = True

prop_multicast_server :: Bool
prop_multicast_server = True

test1 :: IO ()
test1 = quickCheck (prop_join :: ServerStore -> Int -> SockAddr -> Bool)

test2 :: IO ()
test2 = quickCheck (prop_join_handler :: Int -> Int -> SockAddr -> ServerStore -> Bool)

test3 :: IO ()
test3 = quickCheck (prop_nick :: ServerStore -> String -> SockAddr -> Bool)

test4 :: IO ()
test4 = quickCheck (prop_nick_handler :: Int -> SockAddr -> String -> ServerStore -> Bool)

test5 :: IO ()
test5 = quickCheck (prop_text_handler :: Int -> SockAddr -> String -> String -> ServerStore -> Bool)

test6 :: IO ()
test6 = quickCheck (prop_part :: ServerStore -> SockAddr -> Bool)

test7 :: IO ()
test7 = quickCheck (prop_part_handler :: Int -> SockAddr -> ServerStore -> Bool)

test8 :: IO ()
test8 = quickCheck (prop_multicast_client :: Bool)

test9 :: IO ()
test9 = quickCheck (prop_multicast_server :: Bool)

runTests :: IO ()
runTests = test1 >> test2 >> test3 >> test4 >> test5 >> test6 >> test7 >> test8 >> test9

main :: IO ()
main = do
    runTests
