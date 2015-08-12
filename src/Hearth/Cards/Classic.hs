module Hearth.Cards.Classic (
    cards,
) where


--------------------------------------------------------------------------------


import Hearth.Model
import Hearth.Names
import Hearth.Names.Classic hiding (Silence)


--------------------------------------------------------------------------------


cards :: [DeckCard]
cards = [
    abomination,
    aldorPeacekeeper,
    amaniBerserker,
    arcaneGolem,
    argentCommander,
    argentProtector,
    argentSquire,
    battleRage,
    brawl,
    circleOfHealing,
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
    wisp,
    wrath ]


--------------------------------------------------------------------------------


mkMinion :: ClassicCardName -> Mana -> Attack -> Health -> [Ability] -> DeckCard
mkMinion name mana attack health abilities = DeckCardMinion $ Minion' {
    _minionCost = ManaCost mana,
    _minionAttack = attack,
    _minionHealth = health,
    _minionAbilities = abilities,
    _minionName = ClassicCardName name }


mkSpell :: ClassicCardName -> Mana -> SpellEffect -> DeckCard
mkSpell name mana effect = DeckCardSpell $ Spell' {
    _spellCost = ManaCost mana,
    _spellEffect = effect,
    _spellName = ClassicCardName name }


--------------------------------------------------------------------------------


abomination :: DeckCard
abomination = mkMinion Abomination 5 4 4 [
    KeywordAbility Taunt,
    KeywordAbility $ Deathrattle $ \this ->
        All $ Characters [Not (MinionCharacter this)] $ \victims ->
            Effect $ ForEach victims $ \victim ->
                DealDamage victim 2 ]


amaniBerserker :: DeckCard
amaniBerserker = mkMinion AmaniBerserker 2 2 3 [
    KeywordAbility $ Enrage [] [StatsDelta 3 0] ]


aldorPeacekeeper :: DeckCard
aldorPeacekeeper = mkMinion AldorPeacekeeper 3 3 3 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \owner ->
            OpponentOf owner $ \opponent ->
        A $ Minion [OwnedBy opponent] $ \target ->
            Effect $ Enchant target [
                ChangeStat (Left 1) ]]


arcaneGolem :: DeckCard
arcaneGolem = mkMinion ArcaneGolem 3 4 2 [
    KeywordAbility Charge,
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \owner ->
            OpponentOf owner $ \opponent ->
                Effect $ GainManaCrystal CrystalFull opponent ]


argentCommander :: DeckCard
argentCommander = mkMinion ArgentCommander 6 4 2 [
    KeywordAbility Charge,
    KeywordAbility DivineShield ]


argentProtector :: DeckCard
argentProtector = mkMinion ArgentProtector 2 2 2 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \owner ->
            A $ Minion [OwnedBy owner, Not this] $ \target ->
                Effect $ GiveAbility target [KeywordAbility DivineShield ]]


argentSquire :: DeckCard
argentSquire = mkMinion ArgentSquire 1 1 1 [
    KeywordAbility DivineShield ]


battleRage :: DeckCard
battleRage = mkSpell BattleRage 2 $ \this ->
    OwnerOf this $ \owner ->
        All $ Characters [Damaged] $ \characters ->
            Effect $ ForEach characters $ \_ ->
                DrawCards owner 1


brawl :: DeckCard
brawl = mkSpell Brawl 5 $ \_ ->
    Effect $ Elect $ A $ Minion [] $ \survivor ->
        A $ Minion [Not survivor] $ \someNonSurvivor ->
            All $ Minions [Not survivor] $ \victims ->
                Effect $ Sequence [
                    DoNothing someNonSurvivor, -- This is because Brawl requires at least 2 minions to play.
                    ForEach victims $ \victim ->
                        DestroyMinion victim ]


circleOfHealing :: DeckCard
circleOfHealing = mkSpell CircleOfHealing 0 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ ForEach minions $ \minion ->
            RestoreHealth (MinionCharacter minion) 4


coldlightOracle :: DeckCard
coldlightOracle = mkMinion ColdlightOracle 3 2 2[
    KeywordAbility $ Battlecry $ \_ ->
        All $ Players [] $ \players ->
            Effect $ ForEach players $ \player ->
                DrawCards player 2 ]


cruelTaskmaster :: DeckCard
cruelTaskmaster = mkMinion CruelTaskmaster 2 2 2 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Minion [Not this] $ \target ->
            Effect $ Sequence [
                DealDamage (MinionCharacter target) 1,
                Enchant target [StatsDelta 2 0 ]]]


earthenRingFarseer :: DeckCard
earthenRingFarseer = mkMinion EarthenRingFarseer 3 3 3 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \character ->
            Effect $ RestoreHealth character 3 ]


fenCreeper :: DeckCard
fenCreeper = mkMinion FenCreeper 5 3 6 [
    KeywordAbility Taunt ]


injuredBlademaster :: DeckCard
injuredBlademaster = mkMinion InjuredBlademaster 3 4 7 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ DealDamage (MinionCharacter this) 4 ]


ironbeakOwl :: DeckCard
ironbeakOwl = mkMinion IronbeakOwl 2 2 1 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Minion [Not this] $ \target ->
            Effect $ KeywordEffect $ Silence target ]


leperGnome :: DeckCard
leperGnome = mkMinion LeperGnome 1 2 1 [
    KeywordAbility $ Deathrattle $ \this ->
        OwnerOf this $ \owner ->
            OpponentOf owner $ \opponent ->
                Effect $ DealDamage (PlayerCharacter opponent) 2 ]


lootHoarder :: DeckCard
lootHoarder = mkMinion LootHoarder 2 2 1 [
    KeywordAbility $ Deathrattle $ \this ->
        OwnerOf this $ \owner ->
            Effect $ DrawCards owner 1 ]


mogu'shanWarden :: DeckCard
mogu'shanWarden = mkMinion Mogu'shanWarden 4 1 7 [
    KeywordAbility Taunt ]


priestessOfElune :: DeckCard
priestessOfElune = mkMinion PriestessOfElune 6 5 4 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \owner ->
            Effect $ RestoreHealth (PlayerCharacter owner) 4 ]


scarletCrusader :: DeckCard
scarletCrusader = mkMinion ScarletCrusader 3 3 1 [
    KeywordAbility DivineShield ]


silvermoonGuardian :: DeckCard
silvermoonGuardian = mkMinion SilvermoonGuardian 4 3 3 [
    KeywordAbility DivineShield ]


spellbreaker :: DeckCard
spellbreaker = mkMinion Spellbreaker 4 4 3 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Minion [Not this] $ \target ->
            Effect $ KeywordEffect $ Silence target ]


sunwalker :: DeckCard
sunwalker = mkMinion Sunwalker 6 4 5 [
    KeywordAbility Taunt,
    KeywordAbility DivineShield ]


wisp :: DeckCard
wisp = mkMinion Wisp 0 1 1 []


wrath :: DeckCard
wrath = mkSpell Wrath 2 $ \this ->
    Choice [
        A $ Minion [] $ \target ->
            Effect $ DealDamage (MinionCharacter target) 3,
        A $ Minion [] $ \target ->
            OwnerOf this $ \owner ->
                Effect $ Sequence [
                    DealDamage (MinionCharacter target) 1,
                    DrawCards owner 1 ]]










