module Hearth.Authored.HeroPower.Basic.Powers where


import Hearth.Authored.CardSet.Basic.Cards (wickedKnife, silverHandRecruit, healingTotem, searingTotem, stoneclawTotem, wrathOfAirTotem)
import Hearth.Authored.HeroPower.Basic.Names
import Hearth.Combinator.Authoring
import Hearth.Model.Authoring


--------------------------------------------------------------------------------


armorUp :: HeroPower
armorUp = HeroPower {
    _heroPowerName = BasicHeroPowerName ArmorUp,
    _heroPowerCost = ManaCost _2,
    _heroPowerEffect = \you ->
        Effect $ GainArmor you _2 }


daggerMastery :: HeroPower
daggerMastery = HeroPower {
    _heroPowerName = BasicHeroPowerName DaggerMastery,
    _heroPowerCost = ManaCost _2,
    _heroPowerEffect = \you ->
        Effect $ EquipWeapon you wickedKnife }


fireblast :: HeroPower
fireblast = HeroPower {
    _heroPowerName = BasicHeroPowerName Fireblast,
    _heroPowerCost = ManaCost _2,
    _heroPowerEffect = \you ->
        A $ Character [] $ \target ->
            Effect $ DealDamage target _1 (DamagingCharacter $ asCharacter you) }


lesserHeal :: HeroPower
lesserHeal = HeroPower {
    _heroPowerName = BasicHeroPowerName LesserHeal,
    _heroPowerCost = ManaCost _2,
    _heroPowerEffect = \_ ->
        A $ Character [] $ \target ->
            Effect $ RestoreHealth target _2 }


lifeTap :: HeroPower
lifeTap = HeroPower {
    _heroPowerName = BasicHeroPowerName LifeTap,
    _heroPowerCost = ManaCost _2,
    _heroPowerEffect = \you -> 
        Effect $ Sequence [
            DrawCards you 1,
            DealDamage (asCharacter you) _2 (DamagingCharacter $ asCharacter you) ]}


reinforce :: HeroPower
reinforce = HeroPower {
    _heroPowerName = BasicHeroPowerName Reinforce,
    _heroPowerCost = ManaCost _2,
    _heroPowerEffect = \you ->
        Effect $ Summon silverHandRecruit $ Rightmost you }


shapeshift :: HeroPower
shapeshift = HeroPower {
    _heroPowerName = BasicHeroPowerName Shapeshift,
    _heroPowerCost = ManaCost _2,
    _heroPowerEffect = \you ->
        Effect $ Sequence [
            enchant you $ Until EndOfTurn $ gainAttack _1,
            GainArmor you _1 ]}


steadyShot :: HeroPower
steadyShot = HeroPower {
    _heroPowerName = BasicHeroPowerName SteadyShot,
    _heroPowerCost = ManaCost _2,
    _heroPowerEffect = \you ->
        OpponentOf you $ \opponent ->
            Effect $ (you `damages` opponent) _2 }


-- TODO: Not this simple. Needs to select from non-owned totems.
totemicCall :: HeroPower
totemicCall = HeroPower {
    _heroPowerName = BasicHeroPowerName TotemicCall,
    _heroPowerCost = ManaCost _2,
    _heroPowerEffect = \you ->
        Effect $ Get $ ChooseOne' $ map (\minion -> Effect $ (Summon minion) $ Rightmost you) [
            healingTotem,
            searingTotem,
            stoneclawTotem,
            wrathOfAirTotem ]}



