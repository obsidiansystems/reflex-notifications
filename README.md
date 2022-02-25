# reflex-notifications

## Setup
This repo contains one packages: `reflex-notifications`.

An example usage of this packages is available in the `example` directory.

To add this packages to your obelisk project, follow the steps below from your obelisk project root (i.e., the folder you ran `ob init` in).

### Add dependency thunk
```bash
$ mkdir dep
$ cd dep
$ git clone git@github.com:obsidiansystems/reflex-notifications.git
$ ob thunk pack reflex-notifications
```

The last step here (`ob thunk pack`) replaces the cloned repository with a "thunk" that contains all the information obelisk needs to fetch/use the repository when needed.

Check out `ob thunk --help` to learn more about working with thunks.

### Add packages to default.nix

Your skeleton project's `default.nix` uses the [reflex-platform project infrastructure](https://github.com/reflex-frp/reflex-platform/blob/develop/project/default.nix). We can use the [`packages` field](https://github.com/reflex-frp/reflex-platform/blob/develop/project/default.nix#L53-L58) of the project configuration to add our custom packages as follows:

```nix
project ./. ({ hackGet, ... }: {
  packages = {
    reflex-notifications = hackGet ./dep/reflex-notifications;
    ... # other configuration goes here
  };
})
```

Be sure to add `hackGet` to the list of items to bring into scope. `hackGet` is a nix function defined in reflex-platform that takes a path that points to either a source directory or a packed thunk (in other words, it takes a path to a thunk but doesn't care whether it's packed or unpacked). It produces a path to the source (unpacked if necessary). Once we've got that path, we just need to append the subdirectory paths to the individual repos contained in this repository.

### Add packages to cabal files

#### frontend/frontend.cabal
Add `reflex-notifications` to the `build-depends` field of `frontend/frontend.cabal` in `library` stanza.
```cabal
library
  ...
  build-depends: 
    ...
    , reflex-notifications
    ...
  ...
```

## Frontend
On the frontend, use `withUserPermission` and `sendNotification` like the [example](https://github.com/obsidiansystems/reflex-notifications/blob/main/example/frontend/src/Frontend.hs#L40).
```haskell
do
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
```
