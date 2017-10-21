{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}


module Hearth.Heroes (
    anduin,
    garrosh,
    gul'dan,
    jaina,
    malfurion,
    rexxar,
    thrall,
    uther,
    valeera,
) where


--------------------------------------------------------------------------------


import Hearth.Authoring.Combinators
import Hearth.CardSet.Basic.Cards
import Hearth.HeroName
import Hearth.HeroPowerName
import Hearth.Model


--------------------------------------------------------------------------------


mkSimpleHero :: HeroName -> HeroPower -> Hero
mkSimpleHero name power = Hero {
    _heroAttack = 0,
    _heroHealth = 30,
    _heroPower = power,
    _heroName = name }


--------------------------------------------------------------------------------


anduin :: Hero
anduin = mkSimpleHero Anduin lesserHeal


garrosh :: Hero
garrosh = mkSimpleHero Garrosh armorUp


gul'dan :: Hero
gul'dan = mkSimpleHero Gul'dan lifeTap


jaina :: Hero
jaina = mkSimpleHero Jaina fireblast


malfurion :: Hero
malfurion = mkSimpleHero Malfurion shapeshift


rexxar :: Hero
rexxar = mkSimpleHero Rexxar steadyShot


thrall :: Hero
thrall = mkSimpleHero Thrall totemicCall


uther :: Hero
uther = mkSimpleHero Uther reinforce


valeera :: Hero
valeera = mkSimpleHero Valeera daggerMastery


--------------------------------------------------------------------------------


armorUp :: HeroPower
armorUp = HeroPower {
    _heroPowerName = ArmorUp,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you ->
        Effect $ GainArmor you 2 }


daggerMastery :: HeroPower
daggerMastery = HeroPower {
    _heroPowerName = DaggerMastery,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you ->
        Effect $ EquipWeapon you wickedKnife }


fireblast :: HeroPower
fireblast = HeroPower {
    _heroPowerName = Fireblast,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you ->
        A $ Character [] $ \target ->
            Effect $ DealDamage target 1 (DamagingCharacter $ PlayerCharacter you) }


lesserHeal :: HeroPower
lesserHeal = HeroPower {
    _heroPowerName = LesserHeal,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \_ ->
        A $ Character [] $ \target ->
            Effect $ RestoreHealth target 2 }


lifeTap :: HeroPower
lifeTap = HeroPower {
    _heroPowerName = LifeTap,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you -> 
        Effect $ Sequence [
            DrawCards you 1,
            DealDamage (PlayerCharacter you) 2 (DamagingCharacter $ PlayerCharacter you) ]}


reinforce :: HeroPower
reinforce = HeroPower {
    _heroPowerName = Reinforce,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you ->
        Effect $ Summon silverHandRecruit $ Rightmost you }


shapeshift :: HeroPower
shapeshift = HeroPower {
    _heroPowerName = Shapeshift,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you ->
        Effect $ Sequence [
            enchant you $ Until EndOfTurn $ gainAttack 1,
            GainArmor you 1 ]}


steadyShot :: HeroPower
steadyShot = HeroPower {
    _heroPowerName = SteadyShot,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you ->
        OpponentOf you $ \opponent ->
            Effect $ (you `damages` opponent) 2 }


-- TODO: Not this simple. Needs to select from non-owned totems.
totemicCall :: HeroPower
totemicCall = HeroPower {
    _heroPowerName = TotemicCall,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you ->
        Effect $ Get $ ChooseOne' $ map (\minion -> Effect $ (Summon minion) $ Rightmost you) [
            healingTotem,
            searingTotem,
            stoneclawTotem,
            wrathOfAirTotem ]}




