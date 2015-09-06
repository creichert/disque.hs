#!/usr/bin/env stack
-- stack --resolver lts-3.2 --install-ghc runghc --package hedis --package disque

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Database.Disque

main :: IO ()
main = do
  conn <- connect disqueConnectInfo
  runDisque conn $ do
    let queue   = "test_queue"
        data_   = "{ \"foo\": \"bar\" }"
        timeout = 0
    res <- addjob queue data_ timeout
    liftIO (print ("ADDJOB response " ++ (show res)))
    j <- getjob [queue]
    liftIO (print ("GETJOB response: " ++ show j))
