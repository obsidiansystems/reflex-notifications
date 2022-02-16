{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Generated.Static

import Reflex.Dom.Core
-- import Reflex.Notifications

import Common.Api
import Common.Route


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = prerender_ blank notif
  }

notif :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadJSM m, MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m) => RoutedT t (R FrontendRoute) m ()
notif = do
  pure ()
  {-
  (e1, _) <- el' "button" $ text "Click Me"
  (e2, _) <- el' "button" $ text "Click Me 2"
  let
    askPermissionEv = domEvent Click e1
    sendNotificationEv = domEvent Click e2

    options :: NotificationOptions Int
    options = defaultNotificationOptions
      { body = "Heya body"
      }
  
  txtEv <- withUserPermission askPermissionEv
    (do
      withNotification ("Heya title" <$ sendNotificationEv) options $ \notification -> do
        withNotificationEvent notification $ NotificationClick $ consoleLog (s "Heya click")
        withNotificationEvent notification $ NotificationClose $ consoleLog (s "Heya close")
        withNotificationEvent notification $ NotificationShow $ consoleLog (s "Heya show")
        withNotificationEvent notification $ NotificationError $ \err -> do
          txt <- liftJSM $ valToText err
          consoleLog ("Heya error" <> txt)
        pure "Everything works"
      )
    (\err ->
      pure $ show err
      )

  txtDyn <- holdDyn "" $ fmap T.pack txtEv
  el "div" $ dynText txtDyn

  pure ()
  -}
