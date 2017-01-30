{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Spam detection.

module Hpaste.Model.Spam
  where

import           Control.Monad.Env (env)
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.List
import           Data.Monoid
import           Data.String
import qualified Data.Text.Encoding as T
import qualified Data.Trie as Trie
import qualified Database.PostgreSQL.Simple as DB
import           Database.PostgreSQL.Simple hiding (query)
import           Hpaste.Types
import           Snap.App
import           Spam

classifyPaste :: SpamDB -> PasteSubmit -> Double
classifyPaste db = classify db . significantTokens db . makeTokens

-- | Make tokens from a paste submission.
makeTokens :: PasteSubmit -> [ByteString]
makeTokens paste =
  nub
    ((if pasteSubmitTitle paste == "No title"
        then []
        else listTokens t (T.encodeUtf8 (pasteSubmitTitle paste))) <>
     listTokens p (T.encodeUtf8 (pasteSubmitPaste paste)) <>
     (if pasteSubmitAuthor paste == "Anonymous Coward"
        then []
        else listTokens a (T.encodeUtf8 (pasteSubmitAuthor paste))))
  where
    t = 116
    p = 112
    a = 97

-- | Read through undecided pastes and list pastes which seem suspicious.
analyzeSuspicious :: SpamDB -> Model c s ()
analyzeSuspicious db = do
  conn <- env modelStateConn
  Model
    (ReaderT
       (const
          (DB.fold
             conn
             "SELECT id, substring(content for 2000) FROM paste WHERE not spamdecided"
             ()
             ()
             (\() (!id, !paste) ->
                let tokens = listTokens 112 paste
                    rating=classify db (significantTokens db tokens)
                in if rating >= spam
                      then liftIO (print (id::Int,rating,S.take 100 paste))
                      else pure ()))))

-- | Re-generate the spam database based on the postgres database
-- corpus.
generateSpamDB :: Model c s ()
generateSpamDB = do
  !good <-
    queryCorpus
      ["SELECT substring(title for 2000),substring(author for 2000),substring(content for 2000)", "FROM paste", "WHERE NOT flaggedspam"]
      ()
  !bad <-
    queryCorpus
      ["SELECT substring(title for 2000),substring(author for 2000),substring(content for 2000)", "FROM paste", "WHERE flaggedspam"]
      ()
  liftIO (writeDB "spam.db" DB {dbGood = good, dbBad = bad})

-- | Run a query returning a single string field, and build a corpus
-- of messages from each row.
queryCorpus :: (ToRow ps) => [String] -> ps -> Model c s Corpus
queryCorpus q ps = do
  conn <- env modelStateConn
  Model
    (ReaderT
       (const
          (DB.fold
             conn
             (fromString (unlines q))
             ps
             (Corpus 0 Trie.empty)
             (\(!(Corpus !messages !histogram)) (!title, !author, !paste) ->
                return
                  (Corpus
                     (messages + 1)
                     (insertTokens
                        t
                        (insertTokens
                           a
                           (insertTokens p histogram paste)
                           (if author == "Anonymous Coward"
                              then ""
                              else author))
                        (if title == "No title"
                           then ""
                           else title)))))))
  where
    t = 116
    p = 112
    a = 97
