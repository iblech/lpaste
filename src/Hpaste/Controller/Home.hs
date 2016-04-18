{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Home page controller.

module Hpaste.Controller.Home
  (handle)
  where

import Hpaste.Types
import Hpaste.Controller.Cache (cacheIf)
import Hpaste.Controller.Paste (pasteForm)
import Hpaste.Model.Channel    (getChannels)
import Hpaste.Model.Language   (getLanguages)

import Hpaste.Types.Cache      as Key
import Hpaste.View.Home        (page)


import Snap.App

-- | Handle the home page, display a simple list and paste form.
handle :: Bool -> HPCtrl ()
handle spam = do
  html <- cacheIf (not spam) Key.Home $ do
    --  pastes <- model $ getLatestPastes Nothing
    chans <- model $ getChannels
    langs <- model $ getLanguages
    form <- pasteForm chans langs Nothing Nothing Nothing
    uri <- getMyURI
    return $ Just $ page uri chans langs [] form spam
  maybe (return ()) outputText html
