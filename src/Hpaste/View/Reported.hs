{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Reported page view.

module Hpaste.View.Reported
  (page)
  where

import Hpaste.Types
import Hpaste.View.Html
import Hpaste.View.Layout

import Text.Blaze.Pagination
import Data.Monoid.Operator ((++))
import Data.Time.Show       (showDateTime)
import Prelude              hiding ((++))

import Text.Blaze.Html5     as H hiding (map)


-- | Render the reported page.
page :: PN -> [Report] -> String -> Html
page pn rs key =
  layoutPage $ Page {
    pageTitle = "Reported pastes"
  , pageBody = reported pn rs key
  , pageName = "reported"
  }

-- | View the paginated reports.
reported :: PN -> [Report] -> String -> Html
reported pn rs key = do
  darkSection "Reported pastes" $ do
    pagination pn
    table ! aClass "latest-pastes" $ do
      tr $ mapM_ (th . toHtml) $ words "Date Paste Delete Comments"
      reports rs
    pagination pn

    where reports = mapM_ $ \Report{..} -> tr $ do
                      td $ toHtml $ showDateTime reportDate
                      td $ href ("/" ++ show reportPasteId ++ "?show_private=true") $ show reportPasteId
		      td $ href ("/delete?id=" ++ show reportPasteId ++ "&key=" ++ key) ("Delete"::String)
                      td $ toHtml reportComments
