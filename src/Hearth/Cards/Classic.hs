module Hearth.Cards.Classic (
    cards,
) where


--------------------------------------------------------------------------------


import Hearth.Model
import Hearth.Names
import Hearth.Names.Classic


--------------------------------------------------------------------------------


cards :: [DeckCard]
cards = [
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


--------------------------------------------------------------------------------


minion :: ClassicCardName -> Mana -> Attack -> Health -> [Ability] -> DeckCard
minion name mana attack health abilities = DeckCardMinion $ Minion {
    _minionCost = ManaCost mana,
    _minionAttack = attack,
    _minionHealth = health,
    _minionAbilities = abilities,
    _minionName = ClassicCardName name }


--------------------------------------------------------------------------------


amaniBerserker :: DeckCard
amaniBerserker = minion AmaniBerserker 2 2 3 [
    KeywordAbility $ Enrage [] [StatsDelta 3 0] ]


arcaneGolem :: DeckCard
arcaneGolem = minion ArcaneGolem 3 4 2 [
    KeywordAbility Charge,
    KeywordAbility $ Battlecry $ \this ->
        Effect $ With $ Unique $ ControllerOf this $ \controller ->
            With $ Unique $ OpponentOf controller $ \opponent ->
                GainManaCrystal CrystalFull opponent ]


argentCommander :: DeckCard
argentCommander = minion ArgentCommander 6 4 2 [
    KeywordAbility Charge,
    KeywordAbility DivineShield ]


argentProtector :: DeckCard
argentProtector = minion ArgentProtector 2 2 2 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ AnotherFriendlyMinion this $ \target ->
            Effect $ GiveAbility target [KeywordAbility DivineShield]]


argentSquire :: DeckCard
argentSquire = minion ArgentSquire 1 1 1 [
    KeywordAbility DivineShield ]


cruelTaskmaster :: DeckCard
cruelTaskmaster = minion CruelTaskmaster 2 2 2 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ AnotherMinion this $ \target ->
            Effect $ Sequence [
                DealDamage (MinionCharacter target) 1,
                Enchant target [StatsDelta 2 0]]]


ironbeakOwl :: DeckCard
ironbeakOwl = minion IronbeakOwl 2 2 1 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ AnotherMinion this $ \target ->
            Effect $ KeywordEffect $ Silence target ]


scarletCrusader :: DeckCard
scarletCrusader = minion ScarletCrusader 3 3 1 [
    KeywordAbility DivineShield ]


silvermoonGuardian :: DeckCard
silvermoonGuardian = minion SilvermoonGuardian 4 3 3 [
    KeywordAbility DivineShield ]


spellbreaker :: DeckCard
spellbreaker = minion Spellbreaker 4 4 3 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ AnotherMinion this $ \target ->
            Effect $ KeywordEffect $ Silence target ]


sunwalker :: DeckCard
sunwalker = minion Sunwalker 6 4 5 [
    KeywordAbility Taunt,
    KeywordAbility DivineShield ]










