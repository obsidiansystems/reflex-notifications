{-|
Module      : Reflex.Notifications.Types
Description : Provides types for reflex based Notifications API
Copyright   : (c) Obsidian Systems, 2022

This module provides types for reflex based Notifications API, specifically the options
that are passed when creating a new Notification.
-}

{-# LANGUAGE RecordWildCards #-}

module Reflex.Notifications.Types where

import Data.Default
import qualified Data.Text as T
import Language.Javascript.JSaddle

-- | Data type for representing erros
data Error
  = Error_BrowserNotSupported           -- ^ Browser does not support Notifications API
  | Error_UserRejectedPermission        -- ^ User rejected permission to show Notifications
  | Error_DefaultPermissionUnchanged    -- ^ User didn't accept or reject permission, possible on some browsers (Chrome)
  deriving (Eq, Show)

-- | Direction of text inside the Notification
data Direction = LeftToRight | RightToLeft
instance Show Direction where
  show LeftToRight = "ltr"
  show RightToLeft = "rtl"

-- | A type to represent the [Vibration Pattern](https://developer.mozilla.org/en-US/docs/Web/API/Vibration_API#vibration_patterns)
type MilliSecond = Int

-- | Actions to be shown in the Notification
data Action = Action
  { actionText :: T.Text
  , title :: T.Text
  , actionIcon :: T.Text
  }

-- | Notification Options, for details visit <https://developer.mozilla.org/en-US/docs/Web/API/Notification/Notification>
data NotificationOptions a = NotificationOptions
  { dir :: Direction
  , lang :: T.Text
  , badge :: T.Text
  , body :: T.Text
  , tag :: T.Text
  , icon :: T.Text
  , image :: T.Text
  , optionData :: a
  , vibrate :: [MilliSecond]
  , renotify :: Bool
  , requireInteraction :: Bool
  , actions :: [Action]
  , silent :: Bool
  }

-- | A type to represent Notifications
data Notification a = Notification
  { onclick :: Maybe JSCallAsFunction   -- ^ Handler for click event.
  , onclose :: Maybe JSCallAsFunction   -- ^ Handler for close event.
  , onerror :: Maybe JSCallAsFunction   -- ^ Handler for error event.
  , onshow :: Maybe JSCallAsFunction    -- ^ Handler for show event.
  , options :: NotificationOptions a    -- ^ Options for the notification.
  , contents :: T.Text                  -- ^ Text to be shown inside the notification.
  }

-- | Default options for notifications.
defaultNotificationOptions :: (Default a) => NotificationOptions a
defaultNotificationOptions = NotificationOptions
  { dir = LeftToRight
  , lang = T.pack "EN"
  , badge = T.pack ""
  , body = T.pack ""
  , tag = T.pack ""
  , icon = T.pack ""
  , image = T.pack ""
  , optionData = def
  , vibrate = []
  , renotify = False
  , requireInteraction = False
  , actions = []
  , silent = False
  }

instance ToJSVal Action where
  toJSVal Action {..} = do
    o <- obj
    objSetPropertyByName o "action" (show actionText)
    objSetPropertyByName o "title" (show title)
    objSetPropertyByName o "icon" (show actionIcon)
    toJSVal o

instance (ToJSVal a) => ToJSVal (NotificationOptions a) where
  toJSVal NotificationOptions {..} = do
    o <- obj
    objSetPropertyByName o "dir" (show dir)
    objSetPropertyByName o "lang" lang
    objSetPropertyByName o "badge" badge
    objSetPropertyByName o "body" body
    objSetPropertyByName o "tag" tag
    objSetPropertyByName o "icon" icon
    objSetPropertyByName o "image" image
    objSetPropertyByName o "data" =<< toJSVal optionData
    objSetPropertyByName o "vibrate" vibrate
    objSetPropertyByName o "renotify" renotify
    objSetPropertyByName o "requireInteraction" requireInteraction
    objSetPropertyByName o "actions" actions
    objSetPropertyByName o "silent" silent
    toJSVal o
