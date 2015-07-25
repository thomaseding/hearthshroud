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
![screenshot](https://cloud.githubusercontent.com/assets/6971794/8890688/a28745fe-32be-11e5-924f-78451873f826.png)


Goals
* Model cards (and abilities, effects, etc.) as a pure data AST.
* Model enforces game constraints at the type level.
* Engine interprets the cards. (As opposed to the cards directly manipulating the environment.)

