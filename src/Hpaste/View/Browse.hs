{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Browse page view.

module Hpaste.View.Browse
  (page)
  where

import           Hpaste.Types
import           Hpaste.View.Highlight (highlightPaste)
import           Hpaste.View.Html
import           Hpaste.View.Layout
import           Hpaste.View.Paste (pasteLink)
import           Network.URI

import           Control.Monad
import           Data.Maybe
import           Data.Monoid.Operator
import           Data.Pagination
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Time
import           Data.Time.Relative

import           Network.URI.Params
import           Prelude hiding ((++))

import           Text.Blaze.Extra
import           Text.Blaze.Html5 as H hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Pagination

-- | Render the browse page.
page :: UTCTime -> PN -> [Channel] -> [Language] -> [(Paste, Paste)] -> Maybe String -> Html
page now pn chans langs ps mauthor =
  layoutPage $ Page {
    pageTitle = "Browse pastes"
  , pageBody = browse now pn chans langs ps mauthor
  , pageName = "browse"
  }

-- | View the paginated pastes.
browse :: UTCTime -> PN -> [Channel] -> [Language] -> [(Paste, Paste)] -> Maybe String -> Html
browse now pn channels languages ps mauthor = do
  darkSection title $ do
    pagination pn
    mapM_
      (\(original, latest) ->
         (H.div !. "browse-paste")
           (do (H.div !. "browse-paste-title")
                 (pasteLink original (pasteTitle latest))
               (H.div !. "browse-paste-author-channel")
                 (do let authorLatest = T.unpack (pasteAuthor latest)
                         authorOriginal = T.unpack (pasteAuthor original)
                     void "By "
                     if authorLatest == authorOriginal
                       then makeAuthorLink pn authorOriginal
                       else do
                         toMarkup authorLatest
                         void " (original by "
                         makeAuthorLink pn authorOriginal
                         ")"
                     void " in "
                     H.strong
                       (showChannel Nothing channels (pasteChannel latest))
                     void ", "
                     ago (pasteDate original) now)
               (H.a !. "browse-paste-link" ! A.href (toValue ("/" ++ show (pasteId original))))
                 (lightNoTitleSection $
                  highlightPaste
                    languages
                    latest
                    { pastePaste =
                        T.unlines
                          (map
                             (T.take 512)
                             (take 5 (T.lines (T.take 1024 (pastePaste latest)))))
                    })))
      ps
    pagination pn {pnPn = (pnPn pn) {pnShowDesc = False}}
  where
    title =
      LT.pack $
      case mauthor of
        Just author -> "Pastes by " ++ author
        Nothing -> "Latest pastes"

-- | View the paginated pastes.
browse' :: UTCTime -> PN -> [Channel] -> [Language] -> [(Paste, Paste)] -> Maybe String -> Html
browse' now pn channels languages ps mauthor = do
  darkSection title $ do
    pagination pn
    table ! aClass "latest-pastes" $ do
      tr $ mapM_ (th . (toHtml :: String -> Html)) $
         ["Title"] ++ ["Author"|isNothing mauthor] ++ ["When","Language","Channel"]
      pastes ps
    pagination pn { pnPn = (pnPn pn) { pnShowDesc = False } }

    where pastes = mapM_ $ \(original, latest) -> tr $ do
                     td $ pasteLink original (pasteTitle latest)
                     unless (isJust mauthor) $
                       td $ do
                         let authorLatest   = T.unpack (pasteAuthor latest)
                             authorOriginal = T.unpack (pasteAuthor original)
                         if authorLatest == authorOriginal
                            then makeAuthorLink authorOriginal
                            else do toMarkup authorLatest
                                    void " (original by "
                                    makeAuthorLink authorOriginal
                                    ")"
                     td $ ago (pasteDate original) now
                     td $ showLanguage languages (pasteLanguage latest)
                     td $ showChannel Nothing channels (pasteChannel latest)
          makeAuthorLink author
            | True {-validNick author-} = a ! hrefURI (authorUri author)
                                            $ toHtml author
            | otherwise                 = toHtml author
          authorUri author = updateUrlParam "author" author
                           $ updateUrlParam "pastes_page"   "0"
                           $ pnURI pn
          title = LT.pack $ case mauthor of
            Just author -> "Pastes by " ++ author
            Nothing -> "Latest pastes"

makeAuthorLink :: PN -> String -> Html
makeAuthorLink pn author
  | True {-validNick author-} = a ! hrefURI (authorUri pn author)
                                  $ toHtml author
  | otherwise                 = toHtml author

authorUri :: PN -> String -> URI
authorUri pn author = updateUrlParam "author" author
                    $ updateUrlParam "pastes_page"   "0"
                    $ pnURI pn

epoch :: UTCTime -> String
epoch = formatTime defaultTimeLocale "%s"

ago :: UTCTime -> UTCTime -> Html
ago t1 t2 = H.span !. "relative-time"
                   ! dataAttribute "epoch" (toValue (epoch t1))
                   ! A.title (toValue (show t1)) $
   toHtml (relative t1 t2 True)
