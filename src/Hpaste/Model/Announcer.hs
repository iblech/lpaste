{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | IRC announcer.

module Hpaste.Model.Announcer
       (newAnnouncer
       ,announce
       )
       where

import           Hpaste.Types.Announcer
import		 Control.Monad.Fix
import           Control.Concurrent
import qualified Control.Exception       as E
import           Control.Monad

import           Control.Monad.IO        (io)
import qualified Data.ByteString    as B
import           Data.Monoid.Operator    ((++))
import 		 Data.Char
import           Data.Text          (Text,pack,unpack)
import qualified Data.Text          as T
import           Data.Text.Encoding

import qualified Data.Text.IO       as T
import           Network
import           Prelude                 hiding ((++))

import           System.IO

-- | Start a thread and return a channel to it.
newAnnouncer :: AnnounceConfig -> IO Announcer
newAnnouncer config = do
  putStrLn "Connecting..."
  ans <- newChan
  let self = Announcer { annChan = ans, annConfig = config }
  _ <- forkIO $ announcer self (const (return ()))
  return self

-- | Run the announcer bot.
announcer ::  Announcer -> (Handle -> IO ()) -> IO ()
announcer self@Announcer{annConfig=config,annChan=ans} cont = do
  announcements <- getChanContents ans
  forM_ announcements $ \ann ->
    E.catch (sendIfNickExists config ann)
            (\(e::IOError) -> return ())

sendIfNickExists AnnounceConfig{..} (Announcement origin line) = do
  handle <- connectTo announceHost (PortNumber $ fromIntegral announcePort)
  hSetBuffering handle LineBuffering
  let send = B.hPutStrLn handle . encodeUtf8
  send $ "PASS " ++ pack announcePass
  send $ "USER " ++ pack announceUser ++ " * * *"
  send $ "NICK " ++ pack announceUser
  send $ "WHOIS :" ++ origin
  fix $ \loop -> do
    incoming <- T.hGetLine handle
    case T.takeWhile isDigit (T.drop 1 (T.dropWhile (/=' ') incoming)) of
      "311" -> send line
      "401" -> return ()
      _ -> loop

-- | Announce something to the IRC.
announce :: Announcer -> Text -> Text -> Text -> IO ()
announce Announcer{annChan=chan} nick channel line = do
  io $ writeChan chan $ Announcement nick ("PRIVMSG " ++ channel ++ " :" ++ line)
