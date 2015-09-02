
-- | Disque client <https://github.com/antirez/disqueue>

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE CPP                        #-}

module Database.Disque
    (
      Disque
    , runDisque

    , Connection
    , ConnectInfo(..)
    , Reply(..)

    , Job(..)
    , JobId

      -- Create and run disque
    , disqueConnectInfo
    , connect

      -- * Main API
    , addjob
    , getjob
    , getjobs
    , ackjob
    , fastack
    , working
    , nack

      -- * Other Commands
    , info'
    , hello
    , qlen
    , qstat
    , qpeek
    , enqueue
    , dequeue
    , deljob
    , jshow
    , qscan
    , jscan

      -- * High level functions
    , withGetJobs
    ) where

import qualified Data.ByteString.Char8 as BS8

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
import Control.Applicative
#endif

import Control.Monad.IO.Class
import Data.ByteString        (ByteString)
import Database.Redis as R

newtype Disque a
  = Disque (Redis a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadRedis
             )

-- | Run disque transformer
runDisque :: Connection -> Disque a -> IO a
runDisque c (Disque m) = runRedis c m

-- | Disque job
--
-- Job IDs start with a DI and end with an SQ and are always 48 characters
type JobId = ByteString
type Queue = ByteString
type Data  = ByteString

data Job = Job Queue JobId Data
           deriving Show

instance RedisResult Job where
    decode (MultiBulk (Just (x:y:z:_))) =
      Job <$> decode x <*> decode y <*> decode z
    decode r = Left r

-- | Disque connection information
--
-- Use this smart constructor to override specifics
-- to your client connection
--
-- e.g.
--
-- > disqueConnectInfo { connectPort = PortNumber 7712 }
--
disqueConnectInfo :: ConnectInfo
disqueConnectInfo
  = defaultConnectInfo {
      connectHost = "127.0.0.1"
    , connectPort = PortNumber 7711
    }

bshow :: Show a => a -> BS8.ByteString
bshow = BS8.pack . show

addjob :: ByteString -> ByteString -> Int -> Disque (Either Reply ByteString)
addjob q jobdata _timeout = Disque $ sendRequest ["ADDJOB", q, jobdata, "0"]

getjob :: [ByteString] ->  Disque (Either Reply Job)
getjob qs = Disque $ sendRequest $ ["GETJOB", "FROM"] ++ qs

getjobs :: [ByteString] -> Int -> Disque (Either Reply [Job])
getjobs qs cnt = Disque $ sendRequest $ ["GETJOB", "COUNT", bshow cnt, "FROM"] ++ qs

ackjob :: [ByteString] -> Disque (Either Reply ByteString)
ackjob js = Disque $ sendRequest $ ["ACKJOB"] ++ js

fastack :: [ByteString] -> Disque (Either Reply ByteString)
fastack js = Disque $ sendRequest $ ["FASTACK"] ++ js

working :: ByteString -> Disque (Either Reply ByteString)
working jid = Disque $ sendRequest $ ["WORKING", jid]

nack :: [ByteString] -> Disque (Either Reply ByteString)
nack js = Disque $ sendRequest $ ["NACK"] ++ js

info' :: Disque (Either Reply ByteString)
info' = Disque $ sendRequest ["INFO"]

hello :: Disque (Either Reply ByteString)
hello = Disque $ sendRequest ["HELLO"]

qlen :: ByteString -> Disque (Either Reply ByteString)
qlen qname = Disque $ sendRequest ["QLEN", qname]

qstat :: ByteString -> Disque (Either Reply ByteString)
qstat qname = Disque $ sendRequest ["QSTAT", qname]

qpeek :: ByteString -> ByteString -> Disque (Either Reply ByteString)
qpeek qname cnt = Disque $ sendRequest ["QPEEK", qname, cnt]

enqueue :: [ByteString] -> Disque (Either Reply ByteString)
enqueue jids = Disque $ sendRequest $ ["ENQUEUE"] ++ jids

dequeue :: [ByteString] -> Disque (Either Reply ByteString)
dequeue jids = Disque $ sendRequest $ ["DEQUEUE"] ++ jids

deljob :: [ByteString] -> Disque (Either Reply ByteString)
deljob  jids = Disque $ sendRequest $ ["ENQUEUE"] ++ jids

jshow :: ByteString -> Disque (Either Reply ByteString)
jshow jid = Disque $ sendRequest ["SHOW", jid]

qscan :: ByteString -> Disque (Either Reply ByteString)
qscan _ = Disque $ sendRequest ["QSCAN"]

jscan :: ByteString -> Disque (Either Reply ByteString)
jscan _ = Disque $ sendRequest ["JSCAN"]

withGetJobs :: [ByteString] -> Int -> (Job -> Disque a) -> Disque [a]
withGetJobs qs jids f = do
  Right jobs <- getjobs qs jids
  mapM f jobs
