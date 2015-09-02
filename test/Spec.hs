
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

import Data.Text (Text)
import Data.ByteString (ByteString)
import Test.Hspec
import Test.HUnit                     (Assertion, assertBool, assertFailure,
                                       (@=?), (@?=))

import Database.Disque

main :: IO ()
main = do
  conn <- setupDisque
  hspec (specs conn)

dq = runDisque

isRight (Right _) = True
isRight _         = False

specs c = do
  describe "Database.Disque" $ do
    it "Sanity Check" $ do
      assertBool "Sanity Check" True
    it "Adds Jobs" $ do
      pendingWith "Implement disque-server manager."
      -- res <- dq c $ do Right s <- addjob "test_queue" "" 0
      --                  getjob ["test_queue"]
      -- assertBool "Sanity Check" $ isRight res

setupDisque = do
  putStrLn "Starting Disque..."
  conn <- connect $ disqueConnectInfo { connectHost = "192.168.1.104" }
  return conn
