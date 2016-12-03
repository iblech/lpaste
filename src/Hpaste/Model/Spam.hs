{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Spam detection.

module Hpaste.Model.Spam
  where

import           Control.Monad.IO.Class
import           Data.Char
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Hpaste.Types
import           Snap.App
import           Spam

-- | Classify a paste.
classifyPaste :: SpamDB -> PasteSubmit -> Double
classifyPaste db = classify db . makeTokens

-- | Make tokens from a paste submission.
makeTokens :: PasteSubmit -> [Token]
makeTokens p = tokens (pasteSubmitTitle p, pasteSubmitPaste p)

-- | Tokenize a paste.
tokenize :: String -> [Token]
tokenize = map Token . words

-- | Re-generate the spam database based on the postgres database
-- corpus.
generateSpamDB :: Model c s ()
generateSpamDB = do
  good :: [(Text, Text)] <-
    query
      [ "SELECT title, content"
      , "FROM paste"
      , "WHERE NOT flaggedspam"
      , "LIMIT 100"
      ]
      ()
  bad :: [(Text, Text)] <-
    query
      ["SELECT title, content", "FROM paste", "WHERE flaggedspam", "LIMIT 100"]
      ()
  liftIO
    (do writeDB
          "spam.db"
          DB
          { dbGood = corpus tokens good
          , dbBad = corpus tokens bad
          })

-- | Make tokens from paste content.
tokens :: (Text, Text) -> [Token]
tokens  (title, body) =
  map (Token . ("t:" <>)) (chunks (T.unpack title)) <>
  map (Token . ("b:" <>)) (chunks (T.unpack body)) <>
  [Token ("title:" <> T.unpack title), Token ("body:" <> T.unpack body)]
  where
    chunks = words . map replace
      where
        replace c
          | isAlphaNum c || elem c ['$'] = c
          | otherwise = ' '
