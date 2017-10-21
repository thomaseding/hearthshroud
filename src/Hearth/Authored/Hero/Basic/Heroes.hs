{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}


module Hearth.Authored.Hero.Basic.Heroes (
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


import Hearth.Authored.Hero.Basic.Names
import Hearth.Authored.HeroPower.Basic.Powers
import Hearth.Model.Authoring


--------------------------------------------------------------------------------


mkSimpleHero :: BasicHeroName -> HeroPower -> Hero
mkSimpleHero name power = Hero {
    _heroAttack = 0,
    _heroHealth = 30,
    _heroPower = power,
    _heroName = BasicHeroName name }


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




