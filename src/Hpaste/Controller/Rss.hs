{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | An RSS feed of recent pastes.

module Hpaste.Controller.Rss where

import           Hpaste.Model.Paste
import           Hpaste.Model.Channel
import           Hpaste.Types



import           Data.List
import           Data.Monoid

import qualified Data.Text as T
import           Data.Text.Encoding

import           Snap.App
import           Snap.App.RSS

handle :: HPCtrl ()
handle = do
  cid <- getChannelId
  case cid of
    Nothing -> error "need a channel"
    Just cid' -> do
      pastes <- model $ getLatestPastes cid
      outputRSS "#haskell pastes"
                "http://lpaste.net/channel/haskell.rss"
                (map (\Paste{..} -> (pasteDate,pasteTitle,pastePaste, T.pack $
                                    "http://lpaste.net/" ++ show pasteId))
                     pastes)

getChannelId :: HPCtrl (Maybe ChannelId)
getChannelId = do
  chname <- fmap (fmap decodeUtf8) $ getParam "channel"
  case chname of
    Nothing -> return Nothing
    Just chname -> do
      channels <- model $ getChannels
      case find ((==("#" <> chname)) . channelName) channels of
        Nothing -> return Nothing
        Just Channel{..} -> return (return channelId)
