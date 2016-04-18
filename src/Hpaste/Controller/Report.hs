{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- | Report controller.

module Hpaste.Controller.Report
  (handle
  ,handleDelete
  ,handleReportSpam)
  where

import           Hpaste.Controller.Cache (resetCache)
import           Hpaste.Controller.Admin (withAuth)
import           Hpaste.Model.Paste   (getPasteById,deletePaste,markSpamPaste)
import           Hpaste.Model.Report
import           Hpaste.Types
import           Hpaste.Types.Cache      as Key
import           Hpaste.View.Report
import qualified Hpaste.View.Thanks   as Thanks

import           Control.Applicative
import           Control.Monad.Reader
import           Data.ByteString.UTF8 (toString)
import           Data.String
import           Data.Maybe
import           Data.Monoid.Operator ((++))
import           Data.Text            (unpack)
import           Prelude              hiding ((++))
import           Safe
import           Snap.App
import           Text.Blaze.Html5     as H hiding (output,map,body)
import           Text.Formlet

-- | Handle the report/delete page.
handle :: HPCtrl ()
handle = do
   pid <- (>>= readMay) . fmap (toString) <$> getParam "id"
   case pid of
     Nothing -> goHome
     Just (pid :: Integer) -> do
       paste <- model $ getPasteById (PasteId pid)
       (frm,val) <- exprForm
       case val of
	 Just comment -> do
	   _ <- model $ createReport ReportSubmit { rsPaste = PasteId pid
						  , rsComments = comment }
	   resetCache Key.Home
	   output $ Thanks.page "Reported" $
				"Thanks, your comments have " ++
				"been reported to the administrator."
	 Nothing -> maybe goHome (output . page frm) paste

-- | Report form.
exprForm :: HPCtrl (Html,Maybe String)
exprForm = do
  params <- getParams
  submitted <- isJust <$> getParam "submit"
  let formlet = ReportFormlet {
          rfSubmitted = submitted
        , rfParams    = params
        }
      (getValue,_) = reportFormlet formlet
      value = formletValue getValue params
      (_,html) = reportFormlet formlet
      val = either (const Nothing) Just $ value
  return (html,fmap unpack val)

handleDelete :: HPCtrl ()
handleDelete =
  withAuth $ \_ -> do
    pid <- (>>= readMay) . fmap (toString) <$> getParam "id"
    case pid of
      Nothing -> goReport
      Just (pid :: Integer) -> do
        model $ deletePaste pid
        goReport

handleReportSpam :: HPCtrl ()
handleReportSpam =
  withAuth $ \_ -> do
    pid <- (>>= readMay) . fmap (toString) <$> getParam "id"
    case pid of
      Nothing -> goReport
      Just (pid :: Integer) -> do
	model $ markSpamPaste pid
	goReport

-- | Go back to the reported page.
goReport :: HPCtrl ()
goReport = withAuth $ \key -> redirect (fromString ("/reported?key=" ++ key))
