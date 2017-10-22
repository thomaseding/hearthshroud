{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


module Hearth.Combinator.Runtime where


--------------------------------------------------------------------------------


import Hearth.Model.Authoring
import Hearth.Model.Runtime


--------------------------------------------------------------------------------


class CastHandle (a :: ObjectType) where
    castHandle :: Handle b -> Maybe (Handle a)


instance CastHandle 'Spell' where
    castHandle = \case
        h @ SpellHandle {} -> Just h
        _ -> Nothing


instance CastHandle 'Character' where
    castHandle = \case
        SpellHandle {} -> Nothing
        WeaponHandle {} -> Nothing
        h @ MinionHandle {} -> Just $ MinionCharacter h
        h @ PlayerHandle {} -> Just $ PlayerCharacter h
        h @ MinionCharacter {} -> Just h
        h @ PlayerCharacter {} -> Just h


mapHandle :: (Handle 'Spell' -> b) -> (Handle 'Weapon' -> b) -> (Handle 'Minion' -> b) -> (Handle 'Player' -> b) -> (Handle 'Character' -> b) -> (Handle a -> b)
mapHandle spell weapon minion player character h = case h of
    SpellHandle {} -> spell h
    WeaponHandle {} -> weapon h
    MinionHandle {} -> minion h
    PlayerHandle {} -> player h
    MinionCharacter {} -> character h
    PlayerCharacter {} -> character h



