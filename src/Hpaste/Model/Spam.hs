{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Spam detection.

module Hpaste.Model.Spam where

import Hpaste.Types
import Snap.App

-- | Minimum level for something to be considered spam.
spam :: Double
spam = 0.5

-- | Get a spam rating for the given potential paste.
spamRatio :: PasteSubmit -> Model Config s Double
spamRatio _ps = return 0.4
