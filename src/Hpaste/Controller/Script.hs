{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JavaScript controller.

module Hpaste.Controller.Script
  (handle)
  where

import Hpaste.View.Script (script)


import Snap.App

handle :: Controller c s ()
handle = do
  modifyResponse $ setContentType "text/javascript"
  outputText $ script
