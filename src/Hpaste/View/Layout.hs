{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Page layout.

module Hpaste.View.Layout
  (layoutPage)
  where

import           Hpaste.Types
import           Hpaste.View.Html

import           Data.Monoid.Operator        ((++))
import           Prelude                     hiding ((++))
import           Text.Blaze.Html5            as H hiding (map,nav)
import qualified Text.Blaze.Html5.Attributes as A

-- | Render the page in a layout.
layoutPage :: Page -> Markup
layoutPage Page{..} = do
  docTypeHtml $ do
    H.head $ do
      meta ! A.httpEquiv "Content-Type" ! A.content "text/html; charset=UTF-8"
      link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/css/amelie.css"
      js "jquery.js"
      js "amelie.js"
      js "highlight.pack.js"
      title $ toMarkup $ pageTitle ++ " :: lpaste — Lambda pastebin"
      script $
        "hljs.tabReplace = '    ';hljs.initHighlightingOnLoad();"
    body ! A.id (toValue pageName) $ do
      wrap $ do
        nav
        logo
        pageBody
        foot
      preEscapedString "<script type=\"text/javascript\"> var _gaq = _gaq \
                       \|| []; _gaq.push(['_setAccount', 'UA-7443395-10']);\
                       \ _gaq.push(['_trackPageview']); (function() {var ga\
                       \ = document.createElement('script'); ga.type = 'tex\
                       \t/javascript'; ga.async = true; ga.src = ('https:' \
                       \== document.location.protocol ? 'https://ssl' : \
                       \'http://www') + '.google-analytics.com/ga.js'; var\
                       \ s = document.getElementsByTagName('script')[0]; \
                       \s.parentNode.insertBefore(ga, s);})(); </script>"

    where js s = script ! A.type_ "text/javascript"
                        ! A.src ("/js/" ++ s) $
                        return ()

-- | Show the lpaste logo.
logo :: Markup
logo = return ()

-- | Layout wrapper.
wrap :: Markup -> Markup
wrap x = H.div ! aClass "wrap" $ x

-- | Navigation.
nav :: Markup
nav = do
  H.div ! aClass "nav" $ do
    a ! aClass "logo" ! A.href "/" $ "λ"
    a ! A.href "/browse" $ "Browse"
    a ! A.href "mailto:chrisdone@gmail.com" $ "Contact"
    -- " | "
    -- a ! A.href "/activity" $ "Changes"

-- | Page footer.
foot :: Markup
foot = H.div ! aClass "footer" $ p $
  lnk "https://github.com/chrisdone/lpaste" "Web site source code on Github"
  //
  lnk "http://book.realworldhaskell.org/" "Real World Haskell"
  //
  lnk "http://haskell.org/" "Haskell.org"
  //
  lnk "http://planet.haskell.org/" "Planet Haskell"

    where lnk url t = href (url :: String) (t :: String)
          left // right = do _ <- left; (" / " :: Markup); right
