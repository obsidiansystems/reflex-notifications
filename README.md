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
    reflex-notifications = (hackGet ./dep/reflex-notifications) + "/frontend";
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
On the frontend, use `withUserPermission`, `withNotification` and `withNotificationEvent` like the [example](https://github.com/obsidiansystems/reflex-notifications/blob/main/example/frontend/src/Frontend.hs#L51).
```haskell
do
  (e1, _) <- el' "button" $ text "Ask Permission"
  (e2, _) <- el' "button" $ text "Send Notification"
  let
    askPermissionEv = domEvent Click e1
    sendNotificationEv = domEvent Click e2

    options :: NotificationOptions Int
    options = defaultNotificationOptions
      { body = "Heya body"
      , icon = $(static "obelisk.jpg")
      , image = $(static "obelisk.jpg")
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
    (pure . show)

  txtDyn <- holdDyn "" $ fmap T.pack txtEv
  el "div" $ dynText txtDyn

  pure ()
```
