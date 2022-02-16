{-|
Module      : Reflex.Notifications
Description : Provides a reflex based Notifications API
Copyright   : (c) Obsidian Systems, 2022

This module provides functions for dealing with Notifications API in an easy way.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Reflex.Notifications(

  withUserPermission,
  withNotification,
  Notification,
  withNotificationEvent
) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Language.Javascript.JSaddle

import Reflex.Dom.Core hiding (tag)
import Reflex.Notifications.Types
import Reflex.Notifications.Util


checkNotificationPromise :: JSVal -> JSM Bool
checkNotificationPromise notification = do
  maybePromise <- notification ^. js0 (s "requestPermission")
  let
    handler :: JSException -> JSM Bool
    handler = const $ pure False
  flip catch handler $ do
    void $ maybePromise ^. js0 (s "then")
    pure True

getNotification :: (MonadJSM m) => m (Either Error JSVal)
getNotification = liftJSM $ do
  w <- jsg (s "window")
  n <- w ^. js (s "Notification")
  browserNotSupported <- (||) <$> ghcjsPure (isNull n) <*> ghcjsPure (isUndefined n)
  pure $ if browserNotSupported
    then Left Error_BrowserNotSupported
    else Right n

askNotificationPermission
  :: (TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m))
  => Event t ()
  -> m (Event t (Either Error ()))
askNotificationPermission clickEv = do
  performEventAsync $ clickEv <&> \() sendFn -> liftJSM $ do
    getNotification >>= \case
      Left err -> liftIO $ sendFn $ Left err
      Right n -> do
        isPromise <- checkNotificationPromise n
        let
          handler resp = do
            str <- valToText resp
            liftIO $ sendFn $ textToResponse str
        if isPromise
          then do
            prom <- n ^. js0 (s "requestPermission")
            void $ jsThen prom handler (const $ pure ())
          else
            void $ n ^. js1 (s "requestPermission") (fun $ \_ _ [result] -> do
              handler result)
  where
    textToResponse = \case
      "granted" -> Right ()
      "denied"  -> Left Error_UserRejectedPermission
      _         -> Left Error_DefaultPermissionUnchanged

valToNotification
  :: (MonadJSM m)
  => Object
  -> (Event t (), () -> IO ())
  -> (Event t (), () -> IO ())
  -> (Event t T.Text, T.Text -> IO ())
  -> (Event t (), () -> IO ())
  -> m (Notification t)
valToNotification object (onclickEv, clickFn) (oncloseEv, closeFn) (onerrorEv, errorFn) (onshowEv, showFn) = do
  liftJSM $ do
    objSetPropertyByName object (s "onclick") (fun $ \_ _ _ ->
      liftIO $ clickFn ())
    objSetPropertyByName object (s "onclose") (fun $ \_ _ _ ->
      liftIO $ closeFn ())
    objSetPropertyByName object (s "onerror") (fun $ \_ _ [err] -> do
      errText <- valToText err
      liftIO $ errorFn errText)
    objSetPropertyByName object (s "onshow") (fun $ \_ _ _ ->
      liftIO $ showFn ())
  pure $ Notification onclickEv oncloseEv onerrorEv onshowEv

sendNotification
  :: (TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m), ToJSVal a)
  => Event t T.Text
  -> NotificationOptions a
  -> m (Event t (Notification t))
sendNotification msgEv options = do
  (onclickEv, clickFn) <- newTriggerEvent
  (oncloseEv, closeFn) <- newTriggerEvent
  (onerrorEv, errorFn) <- newTriggerEvent
  (onshowEv, showFn) <- newTriggerEvent
  performEvent $ msgEv <&> \msg -> do
    object <- liftJSM $ new (jsg (s "Notification")) [toJSVal msg, toJSVal options] >>= makeObject
    valToNotification object (onclickEv, clickFn) (oncloseEv, closeFn) (onerrorEv, errorFn) (onshowEv, showFn)

-- | Checks if the browser supports Notification API, and then asks user permission to show notifications
-- if the browser supports them.
--
-- If the user approves the permission, a monadic action is run, that results in an event. This plays well with `withNotification`.
--
-- If the user rejects the permission, an error handler is run, that takes an `Reflex.Notifications.Types.Error` and returns a value.
withUserPermission
  :: (TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m), Adjustable t m, MonadHold t m)
  => Event t a        -- ^ The user inititated event used for asking permission from the user
  -> m (Event t b)    -- ^ The monadic action that will be run if the user grants permission
  -> (Error -> m b)   -- ^ Error handler
  -> m (Event t b)
withUserPermission userEv action handler = do
  (errorEv, permissionEv) <- fmap fanEither $ askNotificationPermission $ () <$ userEv
  let
    handledMonadEv = handler <$> errorEv
  ((), handledErrorEv) <- runWithReplace blank handledMonadEv
  ((), actionEventEv) <- runWithReplace blank $ permissionEv <&> \() -> action
  actionEv <- switchHold never actionEventEv
  pure $ leftmost [handledErrorEv, actionEv]

-- | Takes an event containing the text to be shown inside a notification, along with notification options.
--
-- Generates a notification every time the supplied event fires, and then calls a handler on it. This plays well with `withNotificationEvent`.
withNotification
  :: (TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m), Adjustable t m, ToJSVal o)
  => Event t T.Text           -- ^ Event containing the notification text
  -> NotificationOptions o    -- ^ Notification Options
  -> (Notification t -> m a)  -- ^ Function that handles notifications
  -> m (Event t a)
withNotification textEv options f = do
  notificationEv <- sendNotification textEv options
  fmap snd $ runWithReplace blank $ f <$> notificationEv

-- | A type to represent Notifications
data Notification t = Notification
  { onclick :: Event t ()
  , onclose :: Event t ()
  , onerror :: Event t T.Text
  , onshow :: Event t ()
  }

-- | Takes a notification, and event handlers for interacting with a notification.
withNotificationEvent
  :: (Monad m, Adjustable t m)
  => Notification t             -- ^ A notification, generated via `withNotification`.
  -> NotificationAction m a     -- ^ The type of interaction desired with the notification.
  -> m (Event t a)
withNotificationEvent notification = \case
  NotificationClick action -> do
    fmap snd $ runWithReplace blank $ action <$ onclick notification
  NotificationClose action ->
    fmap snd $ runWithReplace blank $ action <$ onclose notification
  NotificationShow action ->
    fmap snd $ runWithReplace blank $ action <$ onshow notification
  NotificationError action ->
    fmap snd $ runWithReplace blank $ action <$> onerror notification
