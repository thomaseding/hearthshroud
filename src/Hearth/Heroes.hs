{-# LANGUAGE ConstraintKinds #-}


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


mkSimpleHero :: (UserConstraint k) => HeroName -> HeroPower k -> Hero k
mkSimpleHero name power = Hero {
    _heroAttack = 0,
    _heroHealth = 30,
    _heroPower = power,
    _heroName = name }


--------------------------------------------------------------------------------


anduin :: (UserConstraint k) => Hero k
anduin = mkSimpleHero Anduin lesserHeal


garrosh :: (UserConstraint k) => Hero k
garrosh = mkSimpleHero Garrosh armorUp


gul'dan :: (UserConstraint k) => Hero k
gul'dan = mkSimpleHero Gul'dan lifeTap


jaina :: (UserConstraint k) => Hero k
jaina = mkSimpleHero Jaina fireblast


malfurion :: (UserConstraint k) => Hero k
malfurion = mkSimpleHero Malfurion shapeshift


rexxar :: (UserConstraint k) => Hero k
rexxar = mkSimpleHero Rexxar steadyShot


thrall :: (UserConstraint k) => Hero k
thrall = mkSimpleHero Thrall totemicCall


uther :: (UserConstraint k) => Hero k
uther = mkSimpleHero Uther reinforce


valeera :: (UserConstraint k) => Hero k
valeera = mkSimpleHero Valeera daggerMastery


--------------------------------------------------------------------------------


armorUp :: (UserConstraint k) => HeroPower k
armorUp = HeroPower {
    _heroPowerName = ArmorUp,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you ->
        Effect $ GainArmor you 2 }


daggerMastery :: (UserConstraint k) => HeroPower k
daggerMastery = HeroPower {
    _heroPowerName = DaggerMastery,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you ->
        Effect $ EquipWeapon you wickedKnife }


fireblast :: (UserConstraint k) => HeroPower k
fireblast = HeroPower {
    _heroPowerName = Fireblast,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you ->
        A $ Character [] $ \target ->
            Effect $ DealDamage target 1 (DamagingCharacter $ PlayerCharacter you) }


lesserHeal :: (UserConstraint k) => HeroPower k
lesserHeal = HeroPower {
    _heroPowerName = LesserHeal,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \_ ->
        A $ Character [] $ \target ->
            Effect $ RestoreHealth target 2 }


lifeTap :: (UserConstraint k) => HeroPower k
lifeTap = HeroPower {
    _heroPowerName = LifeTap,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you -> 
        Effect $ Sequence [
            DrawCards you 1,
            DealDamage (PlayerCharacter you) 2 (DamagingCharacter $ PlayerCharacter you) ]}


reinforce :: (UserConstraint k) => HeroPower k
reinforce = HeroPower {
    _heroPowerName = Reinforce,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you ->
        Effect $ Summon silverHandRecruit $ Rightmost you }


shapeshift :: (UserConstraint k) => HeroPower k
shapeshift = HeroPower {
    _heroPowerName = Shapeshift,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you ->
        Effect $ Sequence [
            Enchant you $ Limited $ Until EndOfTurn $ statsDelta 1 0,
            GainArmor you 1 ]}


steadyShot :: (UserConstraint k) => HeroPower k
steadyShot = HeroPower {
    _heroPowerName = SteadyShot,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you ->
        OpponentOf you $ \opponent ->
            Effect $ (you `damages` opponent) 2 }


-- TODO: Not this simple. Needs to select from non-owned totems.
totemicCall :: (UserConstraint k) => HeroPower k
totemicCall = HeroPower {
    _heroPowerName = TotemicCall,
    _heroPowerCost = ManaCost 2,
    _heroPowerEffect = \you ->
        Effect $ Elect $ ChooseOne' $ map (\minion -> Effect $ (Summon minion) $ Rightmost you) [
            healingTotem,
            searingTotem,
            stoneclawTotem,
            wrathOfAirTotem ]}




