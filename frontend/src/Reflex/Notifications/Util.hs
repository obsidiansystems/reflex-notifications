module Reflex.Notifications.Util where

import Control.Lens
import Control.Monad
import Language.Javascript.JSaddle

s :: String -> String
s = id

consoleLog :: (MonadJSM m, ToJSVal a0) => a0 -> m ()
consoleLog t = liftJSM $ do
  console <- jsg "console"
  void $ console ^. js1 "log" t

jsThen :: (MonadJSM m, MakeObject s) => s -> (JSVal -> JSM ()) -> (JSVal -> JSM ()) -> m JSVal
jsThen promise accept reject = liftJSM $ do
  promise ^. js2 "then"
    (fun $ \_ _ [result] -> do
      accept result)
    (fun $ \_ _ [failure] -> do
      reject failure)
