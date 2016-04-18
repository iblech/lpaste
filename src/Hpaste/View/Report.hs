{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Report view.

module Hpaste.View.Report
  (page,reportFormlet)
  where

import           Hpaste.Types
import           Hpaste.View.Highlight
import           Hpaste.View.Html
import           Hpaste.View.Layout

import           Data.Monoid.Operator        ((++))
import           Data.Text                   (Text)
import           Prelude                     hiding ((++))
import           Text.Blaze.Html5            as H hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Formlet

-- | Render the page page.
page :: Html -> Paste -> Html
page form paste =
  layoutPage $ Page {
    pageTitle = "Report a paste"
  , pageBody = do reporting form; viewPaste paste
  , pageName = "paste"
  }

reporting :: Html -> Html
reporting form = do
  lightSection "Report a paste" $ do
    p $ do "Please put a quick comment for the admin."
    p $ do "If it looks like spam, the admin will mark it as \
           \spam so that the spam filter picks it up in the future."
    p $ do "If the paste contains something private or offensive, \
           \it'll probably just be deleted."
    H.form ! A.method "post" $ do
      form

-- | View a paste's details and content.
viewPaste :: Paste -> Html
viewPaste Paste{..} = do
  pasteDetails pasteTitle
  pasteContent pastePaste

-- | List the details of the page in a dark section.
pasteDetails :: Text -> Html
pasteDetails title =
  darkNoTitleSection $ do
    h2 $ toHtml title
    clear

    where detail title content = do
            li $ do strong (title ++ ":"); content

-- | Show the paste content with highlighting.
pasteContent :: Text -> Html
pasteContent paste =
  lightNoTitleSection $
    highlightHaskell paste

-- | A formlet for report submission / annotating.
reportFormlet :: ReportFormlet -> (Formlet Text,Html)
reportFormlet ReportFormlet{..} =
  let frm = form $ do
        formletHtml reportSubmit rfParams
        submitInput "submit" "Submit"
  in (reportSubmit,frm)

reportSubmit :: Formlet Text
reportSubmit = req (textInput "report" "Comments" (Just "spam"))
