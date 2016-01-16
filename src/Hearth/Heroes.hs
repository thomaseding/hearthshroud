{-# LANGUAGE ConstraintKinds #-}


module Hearth.Heroes (
    jaina,
    gul'dan,
) where


--------------------------------------------------------------------------------


import Hearth.HeroName
import Hearth.HeroPowerName
import Hearth.Model


--------------------------------------------------------------------------------


mkSimpleHero :: (UserConstraint k) => HeroName -> HeroPower k -> Hero k
mkSimpleHero name power = Hero {
    _heroAttack = 0,
    _heroHealth = 30,
    _heroPower = power,
    _heroName = name }


jaina :: (UserConstraint k) => Hero k
jaina = mkSimpleHero Jaina fireblast


gul'dan :: (UserConstraint k) => Hero k
gul'dan = mkSimpleHero Gul'dan lifeTap


fireblast :: (UserConstraint k) => HeroPower k
fireblast = HeroPower {
    _heroPowerName = Fireblast,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you ->
        A $ Character [] $ \target ->
            Effect $ DealDamage target 1 (DamagingCharacter $ PlayerCharacter you) }


lifeTap :: (UserConstraint k) => HeroPower k
lifeTap = HeroPower {
    _heroPowerName = LifeTap,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you -> 
        Effect $ Sequence [
            DrawCards you 1,
            DealDamage (PlayerCharacter you) 2 (DamagingCharacter $ PlayerCharacter you) ]}



