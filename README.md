
# Disque (https://github.com/antirez/disque) client for Haskell

## Getting Started

Run the command line client to get a feel for Disque:

    stack build
	stack exec disque.hs

Integrate `disque.hs` into your code:

    import Control.Monad.IO.Class
    import Database.Disque

    main :: IO ()
    main = do
        conn <- connect disqueConnectInfo
		runDisque conn $ do
		  let queue   = "test_queue"
		      data_   = "{ \"foo\": \"bar\" }"
			  timeout = 0
		  addjob queue data_ timeout
		  j <- getjob q
		  liftIO (print j)

Some commands are not implemented in the test command-line client.
