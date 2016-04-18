module Hpaste.Types.Report where

import Hpaste.Types.Newtypes                   (PasteId)


import Data.Text                               (Text)
import Data.Time                               (UTCTime,zonedTimeToUTC)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Report = Report {
  reportDate :: UTCTime
 ,reportPasteId :: PasteId
 ,reportComments :: Text
} deriving Show

instance FromRow Report where
  fromRow = Report <$> fmap zonedTimeToUTC field
  	    	   <*> field
		   <*> field
