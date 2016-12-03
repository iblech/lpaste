{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Spam detection.

module Hpaste.Model.Spam
  where

import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Char
import           Data.Monoid
import qualified Data.Text.Encoding as T
import           Hpaste.Types
import           Snap.App
import           Spam

-- | Classify a paste.
classifyPaste :: SpamDB -> PasteSubmit -> Double
classifyPaste db = classify db . makeTokens

-- | Make tokens from a paste submission.
makeTokens :: PasteSubmit -> [Token]
makeTokens p =
  tokens
    ( T.encodeUtf8 (pasteSubmitTitle p)
    , T.encodeUtf8 (pasteSubmitPaste p)
    , T.encodeUtf8 (pasteSubmitAuthor p))

-- | Re-generate the spam database based on the postgres database
-- corpus.
generateSpamDB :: Model c s ()
generateSpamDB = do
  good :: [(ByteString, ByteString, ByteString)] <-
    query
      [ "SELECT title, content, author"
      , "FROM paste"
      , "WHERE NOT flaggedspam"
      , "LIMIT 10000"
      ]
      ()
  bad :: [(ByteString, ByteString, ByteString)] <-
    query
      [ "SELECT title, content, author"
      , "FROM paste"
      , "WHERE flaggedspam"
      , "LIMIT 10000"
      ]
      ()
  liftIO
    (do writeDB
          "spam.db"
          DB {dbGood = corpus tokens good, dbBad = corpus tokens bad})

-- | Make tokens from paste content.
tokens :: (ByteString, ByteString, ByteString) -> [Token]
tokens  (title, body, author) =
  map (Token . ("t:" <>)) (chunks title) <>
  map (Token . ("b:" <>)) (chunks body) <>
  map (Token . ("a:" <>)) (chunks author)
  where
    chunks :: ByteString -> [ByteString]
    chunks = S8.words . S8.map replace
      where
        replace c
          | isAlphaNum c || elem c ['$','-','\''] = c
          | otherwise = ' '
