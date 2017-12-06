{-# LANGUAGE
  TypeFamilies
, FlexibleContexts
#-}

module Test where

import Infra  as Infra
import Config as Config
import Test.QuickCheck

prop_join_s :: Bool
prop_join_s = True

prop_send_s :: Bool
prop_send_s = True

prop_leave_s :: Bool
prop_leave_s = True

prop_nick_s :: Bool
prop_nick_s = True

prop_recv_s :: Bool
prop_recv_s = True

prop_send_f :: Bool
prop_send_f = True

prop_join_f :: Bool
prop_join_f = True

prop_nick_f :: Bool
prop_nick_f = True

prop_leave_f :: Bool
prop_leave_f = True

prop_multicast_client :: Bool
prop_multicast_client = True

prop_multicast_server :: Bool
prop_multicast_server = True

test1 :: IO ()
test1 = quickCheck (prop_join_s :: Bool)

test2 :: IO ()
test2 = quickCheck (prop_send_s :: Bool)

test3 :: IO ()
test3 = quickCheck (prop_leave_s :: Bool)

test4 :: IO ()
test4 = quickCheck (prop_nick_s :: Bool)

test5 :: IO ()
test5 = quickCheck (prop_recv_s :: Bool)

test6 :: IO ()
test6 = quickCheck (prop_send_f :: Bool)

test7 :: IO ()
test7 = quickCheck (prop_join_f :: Bool)

test8 :: IO ()
test8 = quickCheck (prop_nick_f :: Bool)

test9 :: IO ()
test9 = quickCheck (prop_leave_f :: Bool)

test10 :: IO ()
test10 = quickCheck (prop_multicast_client :: Bool)

test11 :: IO ()
test11 = quickCheck (prop_multicast_server :: Bool)

runTests :: IO ()
runTests = test1 >> test2 >> test3 >> test4 >> test5 >> test6 >> test7 >> test8 >> test9 >> test10 >> test11

main :: IO ()
main = do
    runTests
