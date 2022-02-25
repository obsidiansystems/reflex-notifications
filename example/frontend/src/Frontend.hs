{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Generated.Static

import Reflex.Dom.Core
import Reflex.Notifications
import Reflex.Notifications.Util

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
  , _frontend_body = prerender_ blank frontendBody
  }

frontendBody :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m) => RoutedT t (R FrontendRoute) m ()
frontendBody = do
  (e1, _) <- el' "button" $ text "Ask Permission"
  (e2, _) <- el' "button" $ text "Send Notification"
  (e3, _) <- el' "button" $ text "Send Notification After 5 seconds"
  let
    notificationOptions :: NotificationOptions Int
    notificationOptions = defaultNotificationOptions
      { body = "Heya body"
      , icon = $(static "obelisk.jpg")
      , image = $(static "obelisk.jpg")
      }

    notification = Notification
      { onclick = Just $ fun $ \_ _ _ -> consoleLog $ s "Heya click"
      , onclose = Just $ fun $ \_ _ _ -> consoleLog $ s "Heya close"
      , onerror = Just $ fun $ \_ _ _ -> consoleLog $ s "Heya error"
      , onshow = Just $ fun $ \_ _ _ -> consoleLog $ s "Heya show"
      , options = notificationOptions
      , contents = "Heya title"
      }
    askPermissionEv = domEvent Click e1
    sendNotificationEv = domEvent Click e2
  sendNotificationAfterEv <- performEventAsync $ domEvent Click e3 <&> \() sendFn -> liftJSM $ do
    void $ jsgf (s "setTimeout")
      ( fun $ \_ _ _ -> do
          liftIO $ sendFn ()
      , 5000 :: Int
      )

  txtEv <- withUserPermission askPermissionEv
    (do
      sendNotification $ notification <$ sendNotificationEv
      sendNotification $ notification <$ sendNotificationAfterEv
      pure "Everything works"
      )
    (pure . show)

  txtDyn <- holdDyn "" $ fmap T.pack txtEv
  el "div" $ dynText txtDyn

  pure ()
