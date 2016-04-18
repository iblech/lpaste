{-# OPTIONS -Wall #-}

-- | The language type.

module Hpaste.Types.Language
       (Language(..))
       where

import Hpaste.Types.Newtypes

import Data.Text                               (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Language = Language {
  languageId    :: LanguageId
 ,languageName  :: Text
 ,languageTitle :: Text
} deriving Show

instance FromRow Language where
  fromRow = Language <$> field
  	    	     <*> field
		     <*> field
