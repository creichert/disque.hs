#!/usr/bin/env stack
-- stack --resolver lts-3.2 --install-ghc runghc --package hedis --package disque

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Database.Disque

main :: IO ()
main = do
  conn <- connect disqueConnectInfo
  runDisque conn $ do
    let timeout = 0
    addjob "test_queue" "test data" timeout >>= liftIO . print
    getjob ["test_queue"] >>= liftIO . print
