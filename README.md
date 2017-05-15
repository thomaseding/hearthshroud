### About

Hearthshroud is one of two things:
 * A Hearthstone game engine library.
 * A simple Hearthstone executable with a console UI.

--------------------

### Library

Library uses a monadic API which drives the game engine for any `HearthMonad`:
```haskell
-- MonadPrompt @ https://hackage.haskell.org/package/MonadPrompt
type UserConstraint k = (k Spell, k Weapon, k Minion, k Player, k Character)
type HearthMonad k m = (UserConstraint k, MonadPrompt (HeartPrompt k) m)
Hearth.Engine.runHearth :: (HearthMonad m) => Pair (PlayerData k) -> m GameResult
```

The `k` constraint is used to make your own evaluators of the Hearthshroud card DSL much simpler to write. It has kind `k :: (* -> Constraint)` If you don't write your own evaluators, then you could leave this value to be generic (or satisfy it with whatever you want).

For an example on how `k` could be used, refer to `Hearth.ShowCard`. Here it fills `k` with a type class to generate handles of type `Handle a` (a GADT which allows `a` to be exactly one of `Spell`, `Weapon`, `Minion`, `Player`, `Character`). When attempting to use `GenHandle a` for an `a` given by some input type `Handle a`, it cannot narrow the choice of `a` to the only possible values. Substituting `k`with `GenHandle` (which the code alias to `Showy`), we now get our instance proven inside the DSL.

For a deeper understanding of the subject, refer to:
https://dorchard.wordpress.com/2011/09/22/constraint-kinds-in-haskell-finally-bringing-us-constraint-families/ Note that this article uses a shallow DSL embedding where as my DSL is deeply embedded, so his syntax is a little different than what you would use with this library. Nonetheless, the idea is the same.

##### Design Choices
 * Model cards (and abilities, effects, etc.) as a pure data AST. This is the card DSL.
 * Model enforces game constraints at the type level.

##### Design Implications 
 * Engine interprets the cards. (As opposed to the cards directly manipulating the environment.)
 * AI directly interprets the same cards as well.

--------------------

### Executable

Sample game client is `Hearth.Client.Console` and can be seen in action by:
```haskell
Hearth.Client.Console.main :: IO ()
```
![ui-game-1](https://cloud.githubusercontent.com/assets/6971794/11055545/16f84d62-872d-11e5-8745-7fcf35add15d.png)
![ui-game-2](https://cloud.githubusercontent.com/assets/6971794/11055306/e807bed6-872a-11e5-9d54-7ffd9a3c7d82.png)
![ui-show-card](https://cloud.githubusercontent.com/assets/6971794/9697842/382720c0-5353-11e5-925b-bbf4665854bf.png)
![ui-game-help](https://cloud.githubusercontent.com/assets/6971794/9697844/84ef6a20-5353-11e5-9e3e-21369cd81479.png)
![ui-program-help](https://cloud.githubusercontent.com/assets/6971794/9697852/df37c482-5353-11e5-862a-4b349f239c11.png)

--------------------

### Installation

#### Steps
 * Clone repository
 * Use `cabal` tool to build/install

For those unfamiliar with `cabal`, search Google for "haskell cabal how to use" (or whatever).

#### Contact Me
 * Probably the best way to ask me a question or tell me a comment is to just create an issue directly for the project (even if it is not a real issue).
 * I am sometimes on `#hearthsim` IRC.

--------------------

### Related Hearthstone Projects

 * http://hearthsim.info/
