{-# OPTIONS -Wall #-}

-- | The channel type.

module Hpaste.Types.Channel
       (Channel(..))
       where

import Hpaste.Types.Newtypes

import Data.Text                               (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Channel = Channel {
  channelId   :: ChannelId
 ,channelName :: Text
} deriving Show

instance FromRow Channel where
  fromRow = Channel <$> field
		    <*> field
