{-|
Module      : Reflex.Notifications.Util
Description : Provides helper functions for reflex based Notifications API
Copyright   : (c) Obsidian Systems, 2022

This module provides helper functions for reflex based Notifications API.
-}

module Reflex.Notifications.Util where

import Control.Lens
import Control.Monad
import Language.Javascript.JSaddle

-- | Helper function to capture String type
s :: String -> String
s = id

-- | Haskell Wrapper of console.log
consoleLog :: (MonadJSM m, ToJSVal a0) => a0 -> m ()
consoleLog t = liftJSM $ do
  console <- jsg "console"
  void $ console ^. js1 "log" t

-- | Haskell Wrapper for [then](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then) function
jsThen :: (MonadJSM m, MakeObject s) => s -> (JSVal -> JSM ()) -> (JSVal -> JSM ()) -> m JSVal
jsThen promise accept reject = liftJSM $ do
  promise ^. js2 "then"
    (fun $ \_ _ [result] -> do
      accept result)
    (fun $ \_ _ [failure] -> do
      reject failure)
