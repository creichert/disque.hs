
-- | Limited disque command line repl in Haskell

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ViewPatterns              #-}

module Main where

import qualified Data.ByteString.Char8 as BS

import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Database.Disque
import System.Console.Haskeline
import System.Environment

data ReplyFmt = Tty
              | Csv
              | Raw

formatReply :: ReplyFmt -> Reply -> BS.ByteString
formatReply _ (SingleLine bs)       = bs
formatReply _ (Error e)             = BS.concat ["(error) ", e]
formatReply _ (Integer i)           = BS.pack $ show i
formatReply _ (Bulk (Just bs))      = bs
formatReply _ (Bulk Nothing)        = "NothingToShow"
formatReply _ (MultiBulk (Just rs)) =
  BS.concat $ [ "1) "] ++ map (formatReply Tty) rs
formatReply _ (MultiBulk Nothing)   = "NothingToShow"

sp :: Eq a => [a] -> [a] -> Maybe [a]
sp = stripPrefix

io :: MonadIO m => IO a -> m a
io = liftIO

split :: String -> [BS.ByteString]
split = filter (not . BS.null) . BS.split ' ' . BS.pack

main :: IO ()
main = do args <- getArgs
          let host = fromMaybe "127.0.0.1" (listToMaybe args)
          conn <- connect $ disqueConnectInfo { connectHost = host }
          runInputT defaultSettings (loop conn)
 where
   p (Right bs)   = io $ BS.putStrLn bs
   p (Left reply) = io $ BS.putStrLn $ formatReply Tty reply
   loop :: Connection -> InputT IO ()
   loop conn = do
     minput <- getInputLine "> "  -- TODO add Host:Port to prompt
     case minput of
       Nothing -> loop conn
       Just ""     -> loop conn
       Just "quit" -> return ()
       Just "help" -> outputStrLn "Command not implemented" >> loop conn
       -- Just ("addjob":ps) -> do
       Just (sp "addjob" -> Just ps) -> do
         --io $ print $ "ADDJOB " ++ show (split ps)
         case split ps of
           [q,d,_c] -> void $ io (runDisque conn (addjob q d 0 >>= p))
           _        -> error "ERR invalid args"
         loop conn
       Just (sp "getjob" -> Just ps) -> do
         case split ps of
           ("from":qs) -> void $ io (runDisque conn (getjob qs >>= io.print))
           _           -> error "ERR invalid args"
         loop conn
       Just "ackjob"  -> do
         io (runDisque conn (ackjob ["jid"] >>= p))
         loop conn
       Just "fastack" -> do
         error "Command not implemented"
       Just "working" -> do
         error "Command not implemented"
       Just "nack" -> do
         io (runDisque conn (nack ["jid"] >>= p))
         loop conn
       Just "info" -> do
         io (runDisque conn (info' >>= p))
         loop conn
       Just "hello" -> do
         io (runDisque conn (hello >>= p))
         loop conn
       Just "qlen"    -> error "Command not implemented"
       Just "qstat"   -> error "Command not implemented"
       Just "qpeek"   -> error "Command not implemented"
       Just "enqueue" -> error "Command not implemented"
       Just "dequeue" -> error "Command not implemented"
       Just "deljob"  -> error "Command not implemented"
       Just "jshow"   -> error "Command not implemented"
       Just "qscan"   -> error "Command not implemented"
       Just "jscan"   -> error "Command not implemented"
       Just c -> do
         outputStrLn ("(error) ERR uknown command '" ++ c ++ "'")
         loop conn
