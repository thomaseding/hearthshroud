module Hearth.Cards.Classic (
    cards,
    minions,
    spells,
) where


--------------------------------------------------------------------------------


import Hearth.Model
import Hearth.Names
import Hearth.Names.Classic


--------------------------------------------------------------------------------


cards :: [DeckCard]
cards = concat [
    map DeckCardMinion minions,
    map DeckCardSpell spells ]


minions :: [Minion]
minions = [
    amaniBerserker,
    arcaneGolem,
    argentCommander,
    argentProtector,
    argentSquire,
    cruelTaskmaster,
    ironbeakOwl,
    scarletCrusader,
    silvermoonGuardian,
    spellbreaker,
    sunwalker ]


spells :: [Spell]
spells = []


--------------------------------------------------------------------------------


minion :: ClassicCardName -> Mana -> Attack -> Health -> [Ability] -> Minion
minion name mana attack health abilities = Minion {
    _minionCost = ManaCost mana,
    _minionAttack = attack,
    _minionHealth = health,
    _minionAbilities = abilities,
    _minionName = ClassicCardName name }


--------------------------------------------------------------------------------


amaniBerserker :: Minion
amaniBerserker = minion AmaniBerserker 2 2 3 [
    KeywordAbility $ Enrage [StatsDelta 3 0] ]


arcaneGolem :: Minion
arcaneGolem = minion ArcaneGolem 3 4 2 [
    KeywordAbility Charge,
    KeywordAbility $ Battlecry $ \this -> With $ ControllerOf this $ \controller -> With $ OpponentOf controller $ \opponent -> GainManaCrystal opponent CrystalFull ]


argentCommander :: Minion
argentCommander = minion ArgentCommander 6 4 2 [
    KeywordAbility Charge,
    KeywordAbility DivineShield ]


argentProtector :: Minion
argentProtector = minion ArgentProtector 2 2 2 [
    KeywordAbility $ Battlecry $ \this -> With $ AnotherMinion this $ \target -> Give target [KeywordAbility DivineShield]]


argentSquire :: Minion
argentSquire = minion ArgentSquire 1 1 1 [
    KeywordAbility DivineShield ]


cruelTaskmaster :: Minion
cruelTaskmaster = minion CruelTaskmaster 2 2 2 [
    KeywordAbility $ Battlecry $ \this -> With $ AnotherMinion this $ \target -> Sequence [
        DealDamage target 1,
        Enchant target [StatsDelta 2 0]]]


ironbeakOwl :: Minion
ironbeakOwl = minion IronbeakOwl 2 2 1 [
    KeywordAbility $ Battlecry $ \this -> With $ AnotherMinion this $ \target -> KeywordEffect $ Silence target ]


scarletCrusader :: Minion
scarletCrusader = minion ScarletCrusader 3 3 1 [
    KeywordAbility DivineShield ]


silvermoonGuardian :: Minion
silvermoonGuardian = minion SilvermoonGuardian 4 3 3 [
    KeywordAbility DivineShield ]


spellbreaker :: Minion
spellbreaker = minion Spellbreaker 4 4 3 [
    KeywordAbility $ Battlecry $ \this -> With $ AnotherMinion this $ \target -> KeywordEffect $ Silence target ]


sunwalker :: Minion
sunwalker = minion Sunwalker 6 4 5 [
    KeywordAbility Taunt,
    KeywordAbility DivineShield ]










