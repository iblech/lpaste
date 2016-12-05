{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Spam detection.

module Spam
  ( SpamDB(..)
  , Corpus(..)
  , readDB
  , writeDB
  , listTokens
  , summarizeDB
  , insertTokens
  , significantTokens
  , classify
  , spam
  ) where

import           Data.Binary
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Trie (Trie)
import qualified Data.Trie as Trie
import qualified Data.Trie.Convenience as Trie
import           GHC.Generics
import           System.Directory

-- | Spam database.
data SpamDB = DB
  { dbBad :: !Corpus
  , dbGood :: !Corpus
  } deriving (Generic)
instance Binary SpamDB

-- | A corpus of pastes.
data Corpus = Corpus
  { corpusMessages :: !Double
  , corpusHistogram :: !(Trie Double)
  } deriving (Show, Generic)
instance Binary Corpus

-- | Read a spam database from file.
readDB :: FilePath -> IO SpamDB
readDB fp = do
  exists <- doesFileExist fp
  if exists
    then do
      result <- decodeFileOrFail fp
      case result of
        Right !db -> return db
        _ -> do
          error "Failed to read spam db."
    else return emptyDB
  where emptyDB = let c = (Corpus 0 Trie.empty)
                  in DB c c

-- | Write the spam database to file.
writeDB :: FilePath -> SpamDB -> IO ()
writeDB fp = encodeFile fp

-- | Print out a summary for the database.
summarizeDB :: SpamDB -> IO ()
summarizeDB (DB bad good) = do
  putStrLn ("Messages: " ++ show (round messageCount :: Int))
  putStrLn
    ("Tokens: " ++
     show tokenCount ++
     " (" ++
     show (Trie.size (corpusHistogram good)) ++
     " ham, " ++ show (Trie.size (corpusHistogram bad)) ++ " spam)")
  putStrLn "Top 20 ham tokens"
  mapM_
    (\(token, count) ->
       putStrLn ("  " ++ S8.unpack token ++ ": " ++ show (round count :: Int)))
    (take
       20
       (sortBy (flip (comparing snd)) (Trie.toList (corpusHistogram good))))
  putStrLn "Top 20 spam tokens"
  mapM_
    (\(token, count) ->
       putStrLn ("  " ++ S8.unpack token ++ ": " ++ show (round count :: Int)))
    (take 20 (sortBy (flip (comparing snd)) (Trie.toList (corpusHistogram bad))))
  where
    messageCount = corpusMessages bad + corpusMessages good
    tokenCount =
      Trie.size (corpusHistogram bad) + Trie.size (corpusHistogram good)

-- | Classify a paste from 0 to 1. >=0.5 being spam.
classify :: SpamDB -> [ByteString] -> Double
classify (DB bad good) = combine . mapMaybe (probability bad good)

-- | Combine the probabilities of n tokens. The probability of a paste
-- being spam.
combine :: [Double] -> Double
combine [] = 0
combine probs = prod / (prod + foldl1' (*) (map (1 -) probs))
  where prod = foldl1' (*) probs

-- | Probability of a token being spam given good and bad
-- corpus. Nothing if we don't know/care.
probability :: Corpus -> Corpus -> ByteString -> Maybe Double
probability bad good token =
  if g + b < occurances
     then Nothing
     else Just
            (max 0.01
                 (min 0.99 ((min 1 (b / nbad)) /
                            (min 1 (g / ngood) + (min 1 (b / nbad))))))
  where g = 2 * fromMaybe 0 (Trie.lookup token (corpusHistogram good))
        b = fromMaybe 0 (Trie.lookup token (corpusHistogram bad))
        ngood = corpusMessages good
        nbad = corpusMessages bad

-- | Insert tokens from @bytes@ into @trie@.
insertTokens :: Word8 -> Trie Double -> ByteString -> Trie Double
insertTokens category trie bytes =
  if S.null bytes
    then trie
    else case S.span constituent bytes of
           (token, rest) ->
             insertTokens
               category
               (if S.length token >= minTokenLen && not (S.all digital token)
                  then Trie.insertWith'
                         (+)
                         (S.cons category (S.cons star token))
                         1
                         trie
                  else trie)
               (S.drop 1 rest)
             where star = 42
{-# INLINE insertTokens #-}

-- | List tokens from @bytes@.
listTokens :: Word8 -> ByteString -> [ByteString]
listTokens category = go []
  where
    go acc bytes =
      if S.null bytes
        then acc
        else case S.span constituent bytes of
               (token, rest) ->
                 go
                   (if S.length token >= minTokenLen &&
                       not (S.all digital token)
                      then (S.cons category (S.cons star token) : acc)
                      else acc)
                   (S.drop 1 rest)
                 where star = 42
{-# INLINE listTokens #-}

-- | Sift out the top significant tokens from the list.
significantTokens :: SpamDB -> [ByteString] -> [ByteString]
significantTokens (DB bad good) =
  map snd .
  take maxSignificantTokens .
  sortBy (flip (comparing fst)) . map (\token -> (rating token, token))
  where
    rating token =
      fromMaybe 0 (Trie.lookup token (corpusHistogram bad)) +
      fromMaybe 0 (Trie.lookup token (corpusHistogram good))

-- | Is the character a constituent?
constituent :: Word8 -> Bool
constituent c =
  c == dollar ||
  c == dash ||
  (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || (c >= 48 && c <= 57)
  where
    dollar = 36
    dash = 45
{-# INLINE constituent #-}

-- | Is a digit?
digital :: Word8 -> Bool
digital c = (c >= 48 && c <= 57)

-- | Number of occurances before we care about a token.
occurances :: Double
occurances = 3 -- Turn this up to 5 when the corpus gets bigger.

-- | Minimum level for something to be considered spam.
spam :: Double
spam = 0.5

-- | Minimum token length, anything smaller is ignored.
minTokenLen :: Int
minTokenLen = 3

-- | Maximum significant tokens to consider in a message that we're
-- testing for spamminess.
maxSignificantTokens :: Int
maxSignificantTokens = 15
