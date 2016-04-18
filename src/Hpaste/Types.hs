{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

-- | All types.

module Hpaste.Types
       (module Hpaste.Types.Paste
       ,module Hpaste.Types.Channel
       ,module Hpaste.Types.Language
       ,module Hpaste.Types.Page
       ,module Hpaste.Types.Newtypes
       ,module Hpaste.Types.Config
       ,module Hpaste.Types.Activity
       ,module Hpaste.Types.Stepeval
       ,module Hpaste.Types.Report
       ,HPState
       ,HPCtrl
       ,HPModel)
       where

import Hpaste.Types.Paste
import Hpaste.Types.Channel
import Hpaste.Types.Language
import Hpaste.Types.Page
import Hpaste.Types.Newtypes
import Hpaste.Types.Config
import Hpaste.Types.Activity
import Hpaste.Types.Stepeval
import Hpaste.Types.Report
import Hpaste.Types.Announcer (Announcer)


import Control.Monad.Env
import Control.Monad.IO
import Control.Monad.Reader

import Snap.App.Types

type HPState = Announcer
type HPCtrl = Controller Config HPState
type HPModel = Model Config HPState

instance AppLiftModel Config HPState where
  liftModel action = do
    conn <- env controllerStateConn
    anns <- env controllerState
    conf <- env controllerStateConfig
    let state = ModelState conn anns conf
    io $ runReaderT (runModel action) state
