{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- | Report controller.

module Hpaste.Controller.Admin
  (withAuth)
  where




import           Hpaste.Types





import           Control.Monad.Reader
import           Data.ByteString.UTF8 (toString)
import           Data.Maybe


import           Prelude              hiding ((++))

import           Snap.App



-- | Do something with authority.
withAuth :: (String -> HPCtrl ()) -> HPCtrl ()
withAuth m = do
  key <- fmap (fmap toString) $ getParam "key"
  realkey <- asks (configKey . controllerStateConfig)
  case key of
    Just k | k == realkey -> m k
    _ -> goHome
