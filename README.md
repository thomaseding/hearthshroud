HearthShroud is a game library and engine for Hearthstone.

Monadic API runs game engine for any `HearthMonad`:
```haskell
type HearthMonad m = MonadPrompt HeartPrompt m  -- https://hackage.haskell.org/package/MonadPrompt
Hearth.Engine.runHearth :: (HearthMonad m) => Pair PlayerData -> m GameResult
```

Sample engine driver is `Hearth.Driver.Driver` and can be seen in action by
```haskell
Hearth.Driver.runTestGame :: IO GameResult
```


Goals
* Model cards (and abilities, effects, etc.) as a pure data AST.
* Model enforces game constraints at the type level.
* Engine interprets the cards. (As opposed to the cards directly manipulating the environment.)

