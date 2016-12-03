{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Spam detection.

module Hpaste.Model.Spam
  where

import           Control.Monad.IO.Class
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
makeTokens p =
  [Token ("t:" ++ T.unpack (pasteSubmitTitle p))
  ,Token ("a:" ++ T.unpack (pasteSubmitAuthor p))] ++
  map (\(Token t) -> Token ("w:" ++ t))
      (tokenize (T.unpack (pasteSubmitPaste p)))

-- | Tokenize a paste.
tokenize :: String -> [Token]
tokenize = map Token . words

-- | Re-generate the spam database based on the postgres database
-- corpus.
generateSpamDB :: Model c s ()
generateSpamDB = do
  good :: [(Text, Text)] <-
    query ["SELECT title, content", "FROM paste", "WHERE NOT flaggedspam"] ()
  bad :: [(Text, Text)] <-
    query ["SELECT title, content", "FROM paste", "WHERE flaggedspam"] ()
  liftIO (do print ("good", length good)
             print ("bad", length bad))
