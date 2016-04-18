{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reported page controller.

module Hpaste.Controller.Reported
  (handle)
  where

import Data.Pagination
import Hpaste.Controller.Admin (withAuth)
import Hpaste.Model.Report (getSomeReports,countReports)
import Hpaste.Types
import Hpaste.View.Reported (page)
import Text.Blaze.Pagination

import Snap.App

-- | List the reported pastes.
handle :: HPCtrl ()
handle =
  withAuth $ \key -> do
    pn <- getPagination "reported"
    total <- model countReports
    reports <- model $ getSomeReports (pnPn pn) {pnTotal = total}
    output $ page pn reports key
