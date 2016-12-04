{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Spam detection.

module Spam
  (SpamDB(..)
  ,Corpus(..)
  ,readDB
  ,writeDB
  ,listTokens
  ,insertTokens
  ,classify
  ,spam
  ,corpus)
  where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Conduit
import           Data.List
import           Data.Maybe
import           Data.Trie (Trie)
import qualified Data.Trie as Trie
import qualified Data.Trie.Convenience as Trie
import           Data.Word
import           System.Directory

-- | Spam database.
data SpamDB = DB
  { dbBad :: !Corpus
  , dbGood :: !Corpus
  }

instance Show SpamDB where
  show (DB (Corpus x a) (Corpus y b)) =
    show (x, Trie.toList a, y, Trie.toList b)

instance Read SpamDB where
  readsPrec _ s = [(v, "")]
    where
      v =
        let (x, a, y, b) = read s
        in DB (Corpus x (Trie.fromList a)) (Corpus y (Trie.fromList b))

-- | A corpus of pastes.
data Corpus = Corpus
  { corpusMessages :: !Double
  , corpusHistogram :: !(Trie Double)
  } deriving (Show)

-- | Read a spam database from file.
readDB :: FilePath -> IO SpamDB
readDB fp = do
  exists <- doesFileExist fp
  if exists
    then do
      content <- readFile fp
      case reads content of
        [(db, "")] -> return db
        _ -> do
          putStrLn "Failed to read spam database. Defaulting to empty one ..."
          return emptyDB
    else return emptyDB
  where emptyDB = let c = (Corpus 0 Trie.empty)
                  in DB c c

-- | Write the spam database to file.
writeDB :: FilePath -> SpamDB -> IO ()
writeDB fp = writeFile fp . show

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

-- | Generate a corpus from a stream of documents.
corpus :: Monad m => Consumer ByteString m Corpus
corpus = go 0 Trie.empty
  where
    go !messages !histogram = do
      result <- await
      case result of
        Nothing -> return (Corpus messages histogram)
        Just message -> go (messages + 1) (insertTokens histogram message)

-- | Insert tokens from @bytes@ into @trie@.
insertTokens :: Trie Double -> ByteString -> Trie Double
insertTokens trie bytes =
  if S.null bytes
    then trie
    else case S.span constituent bytes of
           (token, rest) ->
             insertTokens
               (if not (S.null token)
                  then Trie.insertWith' (+) token 1 trie
                  else trie)
               (S.drop 1 rest)
{-# INLINE insertTokens #-}

-- | List tokens from @bytes@.
listTokens :: ByteString -> [ByteString]
listTokens = go []
  where
    go acc bytes =
      if S.null bytes
        then acc
        else case S.span constituent bytes of
               (token, rest) ->
                 go
                   (if S.null token
                      then acc
                      else (token : acc))
                   (S.drop 1 rest)
{-# INLINE listTokens #-}

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

-- | Number of occurances before we care about a token.
occurances :: Double
occurances = 1 -- Turn this up to 5 when the corpus gets bigger.

-- | Minimum level for something to be considered spam.
spam :: Double
spam = 0.5
