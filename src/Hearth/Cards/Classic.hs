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
    abomination,
    amaniBerserker,
    arcaneGolem,
    argentCommander,
    argentProtector,
    argentSquire,
    coldlightOracle,
    cruelTaskmaster,
    earthenRingFarseer,
    fenCreeper,
    injuredBlademaster,
    ironbeakOwl,
    leperGnome,
    lootHoarder,
    mogu'shanWarden,
    priestessOfElune,
    scarletCrusader,
    silvermoonGuardian,
    spellbreaker,
    sunwalker,
    wisp ]


--------------------------------------------------------------------------------


minion :: ClassicCardName -> Mana -> Attack -> Health -> [Ability] -> DeckCard
minion name mana attack health abilities = DeckCardMinion $ Minion {
    _minionCost = ManaCost mana,
    _minionAttack = attack,
    _minionHealth = health,
    _minionAbilities = abilities,
    _minionName = ClassicCardName name }


--------------------------------------------------------------------------------


abomination :: DeckCard
abomination = minion Abomination 5 4 4 [
    KeywordAbility Taunt,
    KeywordAbility $ Deathrattle $ \this ->
        With $ All $ OtherCharacters (MinionCharacter this) $ \victims ->
            ForEach victims $ \victim ->
                DealDamage victim 2 ]


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


coldlightOracle :: DeckCard
coldlightOracle = minion ColdlightOracle 3 2 2[
    KeywordAbility $ Battlecry $ \_ ->
        Effect $ With $ All $ Players $ \players ->
            ForEach players $ \player ->
                DrawCards player 2 ]


cruelTaskmaster :: DeckCard
cruelTaskmaster = minion CruelTaskmaster 2 2 2 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ AnotherMinion this $ \target ->
            Effect $ Sequence [
                DealDamage (MinionCharacter target) 1,
                Enchant target [StatsDelta 2 0]]]


earthenRingFarseer :: DeckCard
earthenRingFarseer = minion EarthenRingFarseer 3 3 3 [
    KeywordAbility $ Battlecry $ \_ ->
        Targeted $ AnyCharacter $ \character ->
            Effect $ RestoreHealth character 3 ]


fenCreeper :: DeckCard
fenCreeper = minion FenCreeper 5 3 6 [
    KeywordAbility Taunt ]


injuredBlademaster :: DeckCard
injuredBlademaster = minion InjuredBlademaster 3 4 7 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ DealDamage (MinionCharacter this) 4 ]


ironbeakOwl :: DeckCard
ironbeakOwl = minion IronbeakOwl 2 2 1 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ AnotherMinion this $ \target ->
            Effect $ KeywordEffect $ Silence target ]


leperGnome :: DeckCard
leperGnome = minion LeperGnome 1 2 1 [
    KeywordAbility $ Deathrattle $ \this ->
        With $ Unique $ ControllerOf this $ \controller ->
            With $ Unique $ OpponentOf controller $ \opponent ->
                DealDamage (PlayerCharacter opponent) 2 ]


lootHoarder :: DeckCard
lootHoarder = minion LootHoarder 2 2 1 [
    KeywordAbility $ Deathrattle $ \this ->
        With $ Unique $ ControllerOf this $ \controller ->
            DrawCards controller 1 ]


mogu'shanWarden :: DeckCard
mogu'shanWarden = minion Mogu'shanWarden 4 1 7 [
    KeywordAbility Taunt ]


priestessOfElune :: DeckCard
priestessOfElune = minion PriestessOfElune 6 5 4 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ With $ Unique $ ControllerOf this $ \controller ->
            RestoreHealth (PlayerCharacter controller) 4 ]


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


wisp :: DeckCard
wisp = minion Wisp 0 1 1 []










