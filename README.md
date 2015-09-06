
# Disque client for Haskell

[disque][https://github.com/antirez/disque] is a distributed, in
memory, message broker.

## Getting Started

Run the command line client to get a feel for Disque:

    stack build
	stack exec disque.hs

Integrate `disque.hs` into your code:

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


Some commands are not implemented in the test command-line client.
