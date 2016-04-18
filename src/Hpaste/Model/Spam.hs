{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Spam detection.

module Hpaste.Model.Spam
  (SpamDB
  ,readDB
  ,classify
  ,spam)
  where

import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Hpaste.Types
import           System.Directory

-- | A token from a document.
newtype Token = Token String
  deriving (Ord,Eq,Show,Read)

-- | Spam database.
data SpamDB =
  DB {dbBad :: !Corpus
     ,dbGood :: !Corpus}
       deriving (Read,Show)

instance Monoid SpamDB where
  mempty = DB mempty mempty
  mappend (DB a x) (DB b y) = DB (a <> b) (x <> y)

-- | A corpus of pastes.
data Corpus =
  Corpus {corpusMessages :: Double
         ,corpusHistogram :: Map Token Double}
  deriving (Show,Read)

instance Monoid Corpus where
  mempty = Corpus 0 mempty
  mappend (Corpus a x) (Corpus b y) =
    Corpus (a + b) (M.unionWith (+) x y)

-- | Read a spam database from file.
readDB :: FilePath -> IO SpamDB
readDB fp =
  do exists <- doesFileExist fp
     if exists
        then do content <- readFile fp
                case reads content of
                  [(db,"")] -> return db
                  _ ->
                    do putStrLn "Failed to read spam database. Defaulting to empty one ..."
                       return mempty
        else return mempty

-- | Classify a paste from 0 to 1. >=0.5 being spam.
classify :: SpamDB -> PasteSubmit -> Double
classify (DB bad good) = combine . mapMaybe (probability bad good) . makeTokens
  where makeTokens p =
          [Token ("t:" ++ T.unpack (pasteSubmitTitle p))
          ,Token ("a:" ++ T.unpack (pasteSubmitAuthor p))] ++
          map (\(Token t) -> Token ("w:" ++ t))
              (tokenize (T.unpack (pasteSubmitPaste p)))

-- | Combine the probabilities of n tokens. The probability of a paste
-- being spam.
combine :: [Double] -> Double
combine [] = 0
combine probs = prod / (prod + foldl1' (*) (map (1 -) probs))
  where prod = foldl1' (*) probs

-- | Probability of a token being spam given good and bad
-- corpus. Nothing if we don't know/care.
probability :: Corpus -> Corpus -> Token -> Maybe Double
probability bad good token =
  if g + b < occurances
     then Nothing
     else Just
            (max 0.01
                 (min 0.99 ((min 1 (b / nbad)) /
                            (min 1 (g / ngood) + (min 1 (b / nbad))))))
  where g = 2 * M.findWithDefault 0 token (corpusHistogram good)
        b = M.findWithDefault 0 token (corpusHistogram bad)
        ngood = corpusMessages good
        nbad = corpusMessages bad

-- | Minimum level for something to be considered spam.
spam :: Double
spam = 0.5

-- | Generate a corpus from a set of pastes.
corpus :: [String] -> Corpus
corpus = foldl' (<>) mempty . map (Corpus 1 . histogram . tokenize)

-- | Tokenize a paste.
tokenize :: String -> [Token]
tokenize = map Token . words

-- | Generate a histogram from a list of tokens.
histogram :: [Token] -> Map Token Double
histogram = foldl' (\m t -> M.insertWith (+) t 1 m) mempty

-- | Number of occurances before we care about a token.
occurances :: Double
occurances = 1 -- Turn this up to 5 when the corpus gets bigger.
