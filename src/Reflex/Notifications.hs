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
  sendNotification,
  module Reflex.Notifications.Types
) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
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

-- | Takes an event of notifications. Whenever it receives a `Notification`, it sends
-- a notification to the browser.
sendNotification
  :: (PerformEvent t m, MonadJSM (Performable m), ToJSVal a)
  => Event t (Notification a)
  -> m ()
sendNotification notificationEv = do
  performEvent_ $ notificationEv <&> \notification -> liftJSM $ do
    object <- new (jsg (s "Notification")) [toJSVal $ contents notification, toJSVal $ options notification] >>= makeObject
    forM_ (onclick notification) $ \handler ->
      objSetPropertyByName object (s "onclick") handler
    forM_ (onclose notification) $ \handler ->
      objSetPropertyByName object (s "onclose") handler
    forM_ (onerror notification) $ \handler ->
      objSetPropertyByName object (s "onerror") handler
    forM_ (onshow notification) $ \handler ->
      objSetPropertyByName object (s "onshow") handler

-- | Checks if the browser supports Notification API, and then asks user permission to show notifications
-- if the browser supports them.
--
-- If the user approves the permission, a monadic action is run. This plays well with `sendNotification`.
--
-- If the user rejects the permission, an error handler is run, that takes an `Reflex.Notifications.Types.Error` and returns a value.
withUserPermission
  :: (TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m), Adjustable t m)
  => Event t a        -- ^ The user inititated event used for asking permission from the user
  -> m b              -- ^ The monadic action that will be run if the user grants permission
  -> (Error -> m b)   -- ^ Error handler
  -> m (Event t b)
withUserPermission userEv action handler = do
  (errorEv, permissionEv) <- fmap fanEither $ askNotificationPermission $ () <$ userEv
  let
    handledMonadEv = handler <$> errorEv
  ((), handledErrorEv) <- runWithReplace blank handledMonadEv
  ((), actionEv) <- runWithReplace blank $ action <$ permissionEv
  pure $ leftmost [handledErrorEv, actionEv]
