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
import           Data.Monoid
import           Data.String
import qualified Data.Text.Encoding as T
import qualified Data.Trie as Trie
import qualified Database.PostgreSQL.Simple as DB
import           Database.PostgreSQL.Simple hiding (query)
import           Hpaste.Types
import           Snap.App
import           Spam

-- | Classify a paste.
classifyPaste :: SpamDB -> PasteSubmit -> Double
classifyPaste db = classify db . makeTokens

-- | Make tokens from a paste submission.
makeTokens :: PasteSubmit -> [ByteString]
makeTokens p =
  listTokens (T.encodeUtf8 (pasteSubmitTitle p)) <>
  listTokens (T.encodeUtf8 (pasteSubmitPaste p)) <>
  listTokens (T.encodeUtf8 (pasteSubmitAuthor p))

-- | Re-generate the spam database based on the postgres database
-- corpus.
generateSpamDB :: Model c s ()
generateSpamDB = do
  !good <-
    queryCorpus
      ["SELECT substring(content for 2000)", "FROM paste", "WHERE NOT flaggedspam"]
      ()
  !bad <-
    queryCorpus
      ["SELECT substring(content for 2000)", "FROM paste", "WHERE flaggedspam"]
      ()
  liftIO (writeDB "spam.db" DB {dbGood = good, dbBad = bad})

-- | Run a query returning a single string field, and build a corpus
-- of messages from each row.
queryCorpus :: (ToRow ps) => [String] -> ps -> Model c s Corpus
queryCorpus q ps = do
  conn <- env modelStateConn
  Model
    (ReaderT
       (\_ -> do
          (DB.fold
             conn
             (fromString (unlines q))
             ps
             (Corpus 0 Trie.empty)
             (\(!(Corpus !messages !histogram)) (Only !message) ->
                return (Corpus (messages + 1) (insertTokens histogram message))))))
