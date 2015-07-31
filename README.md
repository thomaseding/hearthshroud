Hearthshroud is a game library and engine for Hearthstone.

Monadic API runs game engine for any `HearthMonad`:
```haskell
type HearthMonad m = MonadPrompt HeartPrompt m  -- https://hackage.haskell.org/package/MonadPrompt
Hearth.Engine.runHearth :: (HearthMonad m) => Pair PlayerData -> m GameResult
```

Sample game client is `Hearth.Client.Console` and can be seen in action by
```haskell
Hearth.Client.Console.main :: IO ()
```
![screenshot](https://cloud.githubusercontent.com/assets/6971794/9003372/5e749994-3722-11e5-8bb1-d0e7b80e8909.png)


Goals
* Model cards (and abilities, effects, etc.) as a pure data AST.
* Model enforces game constraints at the type level.
* Engine interprets the cards. (As opposed to the cards directly manipulating the environment.)

