{-# LANGUAGE NoMonomorphismRestriction #-}


module Hearth.Set.Classic.Cards (
    cards,
) where


--------------------------------------------------------------------------------


import Hearth.Authoring.Combinators
import Hearth.Model
import Hearth.CardName
import Hearth.Set.Classic.Names hiding (Silence)
import qualified Hearth.Set.Classic.Names as Classic


--------------------------------------------------------------------------------


cards :: [DeckCard]
cards = let x = toCard in [
    x abomination,
    x abusiveSergeant,
    x aldorPeacekeeper,
    x amaniBerserker,
    x arcaneGolem,
    x argentCommander,
    x argentProtector,
    x argentSquire,
    x armorsmith,
    x battleRage,
    x bigGameHunter,
    x blessedChampion,
    x brawl,
    x circleOfHealing,
    x coldlightOracle,
    x crazedAlchemist,
    x cruelTaskmaster,
    x direWolfAlpha,
    x earthenRingFarseer,
    x earthShock,
    x equality,
    x fenCreeper,
    x flameImp,
    x gadgetzanAuctioneer,
    x grommashHellscream,
    x holyFire,
    x injuredBlademaster,
    x innerRage,
    x ironbeakOwl,
    x layOnHands,
    x leperGnome,
    x lootHoarder,
    x markOfNature,
    x massDispel,
    x mogu'shanWarden,
    x naturalize,
    x nourish,
    x pitLord,
    x priestessOfElune,
    x pyroblast,
    x rampage,
    x scarletCrusader,
    x shieldbearer,
    x silence,
    x silvermoonGuardian,
    x siphonSoul,
    x spellbreaker,
    x stampedingKodo,
    x starfall,
    x sunwalker,
    x taurenWarrior,
    x templeEnforcer,
    x twistingNether,
    x wisp,
    x wrath ]


--------------------------------------------------------------------------------


mkMinion :: Rarity -> Class -> ClassicCardName -> Mana -> Attack -> Health -> [Ability] -> Minion
mkMinion = mkMinion' ClassicCardName


mkSpell :: Rarity -> Class -> ClassicCardName -> Mana -> SpellEffect -> Spell
mkSpell = mkSpell' ClassicCardName


--------------------------------------------------------------------------------


abomination :: Minion
abomination = mkMinion Rare Neutral Abomination 5 4 4 [
    KeywordAbility Taunt,
    KeywordAbility $ Deathrattle $ \this ->
        All $ Characters [Not (MinionCharacter this)] $ \victims ->
            Effect $ ForEach victims $ \victim ->
                (this `damages` victim) 2 ]


abusiveSergeant :: Minion
abusiveSergeant = mkMinion Common Neutral AbusiveSergeant 1 2 1 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Minion [Not this] $ \target ->
            Effect $ Enchant target $ Limited $ Until EndOfTurn $ StatsDelta 2 0 ]


amaniBerserker :: Minion
amaniBerserker = mkMinion Common Neutral AmaniBerserker 2 2 3 [
    KeywordAbility $ Enrage [] [
        StatsDelta 3 0 ]]


aldorPeacekeeper :: Minion
aldorPeacekeeper = mkMinion Rare Paladin AldorPeacekeeper 3 3 3 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                A $ Minion [OwnedBy opponent] $ \target ->
                    Effect $ Enchant target $ Continuous $ ChangeStat (Left 1) ]


arcaneGolem :: Minion
arcaneGolem = mkMinion Rare Neutral ArcaneGolem 3 4 2 [
    KeywordAbility Charge,
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ GainManaCrystals opponent 1 CrystalFull ]


argentCommander :: Minion
argentCommander = mkMinion Rare Neutral ArgentCommander 6 4 2 [
    KeywordAbility Charge,
    KeywordAbility DivineShield ]


argentProtector :: Minion
argentProtector = mkMinion Common Paladin ArgentProtector 2 2 2 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion [OwnedBy you, Not this] $ \target ->
                Effect $ GrantAbilities target [
                    KeywordAbility DivineShield ]]


argentSquire :: Minion
argentSquire = mkMinion Common Neutral ArgentSquire 1 1 1 [
    KeywordAbility DivineShield ]


armorsmith :: Minion
armorsmith = mkMinion Rare Warrior Armorsmith 2 1 4 [
    Whenever $ \this ->
        DamageIsDealt $ \victim _ _ ->
            OwnerOf this $ \you ->
                Effect $ when (victim `Satisfies` [OwnedBy you, IsMinion]) $ GainArmor you 1 ]


battleRage :: Spell
battleRage = mkSpell Common Warrior BattleRage 2 $ \this ->
    OwnerOf this $ \you ->
        All $ Characters [Damaged, OwnedBy you] $ \friendlies ->
            Effect $ ForEach friendlies $ \_ ->
                DrawCards you 1


bigGameHunter :: Minion
bigGameHunter = mkMinion Epic Neutral BigGameHunter 3 4 2 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Minion [Not this, AttackCond GreaterEqual 7] $ \target ->
            Effect $ DestroyMinion target ]


blessedChampion :: Spell
blessedChampion = mkSpell Rare Paladin BlessedChampion 5 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ StatsScale 2 1


brawl :: Spell
brawl = mkSpell Epic Warrior Brawl 5 $ \_ ->
    Effect $ Elect $ A $ Minion [] $ \survivor ->
        A $ Minion [Not survivor] $ \someNonSurvivor ->
            All $ Minions [Not survivor] $ \victims ->
                Effect $ Sequence [
                    Unreferenced someNonSurvivor, -- This is because Brawl requires at least 2 minions to play.
                    ForEach victims $ \victim ->
                        DestroyMinion victim ]


circleOfHealing :: Spell
circleOfHealing = mkSpell Common Priest CircleOfHealing 0 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ ForEach minions $ \minion ->
            RestoreHealth (MinionCharacter minion) 4


coldlightOracle :: Minion
coldlightOracle = mkMinion Rare Neutral ColdlightOracle 3 2 2[
    KeywordAbility $ Battlecry $ \_ ->
        All $ Players [] $ \players ->
            Effect $ ForEach players $ \player ->
                DrawCards player 2 ]


crazedAlchemist :: Minion
crazedAlchemist = mkMinion Rare Neutral CrazedAlchemist 2 2 2 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Minion [Not this] $ \target ->
            Effect $ Enchant target $ Continuous SwapStats ]


cruelTaskmaster :: Minion
cruelTaskmaster = mkMinion Common Warrior CruelTaskmaster 2 2 2 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Minion [Not this] $ \target ->
            Effect $ Sequence [
                (this `damages` target) 1,
                Enchant target $ Continuous $ StatsDelta 2 0 ]]


direWolfAlpha :: Minion
direWolfAlpha = mkMinion Common Neutral DireWolfAlpha 2 2 2 [
    Aura $ \this ->
        EachMinion [AdjacentTo this] $ \minion ->
            Has minion $ StatsDelta 1 0 ]


earthenRingFarseer :: Minion
earthenRingFarseer = mkMinion Common Neutral EarthenRingFarseer 3 3 3 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \character ->
            Effect $ RestoreHealth character 3 ]


earthShock :: Spell
earthShock = mkSpell Common Shaman EarthShock 1 $ \this ->
    A $ Minion [] $ \target ->
        Effect $ Sequence [
            Silence target,
            (this `damages` target) 1 ]


equality :: Spell
equality = mkSpell Rare Paladin Equality 2 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ ForEach minions $ \minion ->
            Enchant minion $ Continuous $ ChangeStat (Right 1)


fenCreeper :: Minion
fenCreeper = mkMinion Common Neutral FenCreeper 5 3 6 [
    KeywordAbility Taunt ]


flameImp :: Minion
flameImp = mkMinion Common Warlock FlameImp 1 3 2 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ (this `damages` you) 3 ]


gadgetzanAuctioneer :: Minion
gadgetzanAuctioneer = mkMinion Rare Neutral GadgetzanAuctioneer 6 4 4 [
    Whenever $ \this ->
        SpellIsCast $ \spell ->
            OwnerOf this $ \you ->
                Effect $ when (spell `Satisfies` [OwnedBy you]) $ DrawCards you 1 ]


grommashHellscream :: Minion
grommashHellscream = mkMinion Legendary Warrior GrommashHellscream 8 4 9 [
    KeywordAbility Charge,
    KeywordAbility $ Enrage [] [
        StatsDelta 6 0 ]]


holyFire :: Spell
holyFire = mkSpell Rare Priest HolyFire 6 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 5,
                RestoreHealth (PlayerCharacter you) 5 ]


injuredBlademaster :: Minion
injuredBlademaster = mkMinion Rare Neutral InjuredBlademaster 3 4 7 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ (this `damages` this) 4 ]


innerRage :: Spell
innerRage = mkSpell Common Warrior InnerRage 0 $ \this ->
    A $ Minion [] $ \target ->
        Effect $ Sequence [
            (this `damages` target) 1,
            Enchant target $ Continuous $ StatsDelta 2 0 ]


ironbeakOwl :: Minion
ironbeakOwl = mkMinion Common Neutral IronbeakOwl 2 2 1 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Minion [Not this] $ \target ->
            Effect $ Silence target ]


layOnHands :: Spell
layOnHands = mkSpell Epic Paladin LayOnHands 8 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                RestoreHealth target 6,
                DrawCards you 3 ]


leperGnome :: Minion
leperGnome = mkMinion Common Neutral LeperGnome 1 2 1 [
    KeywordAbility $ Deathrattle $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ (this `damages` opponent) 2 ]


lootHoarder :: Minion
lootHoarder = mkMinion Common Neutral LootHoarder 2 2 1 [
    KeywordAbility $ Deathrattle $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


markOfNature :: Spell
markOfNature = mkSpell Common Druid MarkOfNature 3 $ \_ ->
    Choice [
        A $ Minion [] $ \target ->
            Effect $ Enchant target $ Continuous $ StatsDelta 4 0,
        A $ Minion [] $ \target ->
            Effect $ Sequence [
                Enchant target $ Continuous $ StatsDelta 0 4,
                GrantAbilities target [
                    KeywordAbility Taunt ]]]


massDispel :: Spell
massDispel = mkSpell Rare Priest MassDispel 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ Sequence [
                    ForEach victims $ \victim ->
                        Silence victim,
                    DrawCards you 1 ]


mogu'shanWarden :: Minion
mogu'shanWarden = mkMinion Common Neutral Mogu'shanWarden 4 1 7 [
    KeywordAbility Taunt ]


naturalize :: Spell
naturalize = mkSpell Common Druid Naturalize 1 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            A $ Minion [] $ \target ->
                Effect $ Sequence [
                    DestroyMinion target,
                    DrawCards opponent 2 ]


nourish :: Spell
nourish = mkSpell Rare Druid Nourish 5 $ \this ->
    OwnerOf this $ \you ->
        Choice [
            Effect $ GainManaCrystals you 2 CrystalFull,
            Effect $ DrawCards you 3 ]


pitLord :: Minion
pitLord = mkMinion Epic Warlock PitLord 4 5 6 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ (this `damages` you) 5 ]


priestessOfElune :: Minion
priestessOfElune = mkMinion Common Neutral PriestessOfElune 6 5 4 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ RestoreHealth (PlayerCharacter you) 4 ]


pyroblast :: Spell
pyroblast = mkSpell Epic Mage Pyroblast 10 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) 10


rampage :: Spell
rampage = mkSpell Common Warrior Rampage 2 $ \_ ->
    A $ Minion [WithMinion Damaged] $ \target ->
        Effect $ Enchant target $ Continuous $ StatsDelta 3 3


scarletCrusader :: Minion
scarletCrusader = mkMinion Common Neutral ScarletCrusader 3 3 1 [
    KeywordAbility DivineShield ]


shieldbearer :: Minion
shieldbearer = mkMinion Common Neutral Shieldbearer 1 0 4 [
    KeywordAbility Taunt ]


silence :: Spell
silence = mkSpell Common Priest Classic.Silence 0 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Silence target


silvermoonGuardian :: Minion
silvermoonGuardian = mkMinion Common Neutral SilvermoonGuardian 4 3 3 [
    KeywordAbility DivineShield ]


siphonSoul :: Spell
siphonSoul = mkSpell Rare Warlock SiphonSoul 6 $ \this ->
    A $ Minion [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                DestroyMinion target,
                RestoreHealth (PlayerCharacter you) 3 ]


spellbreaker :: Minion
spellbreaker = mkMinion Common Neutral Spellbreaker 4 4 3 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Minion [Not this] $ \target ->
            Effect $ Silence target ]


stampedingKodo :: Minion
stampedingKodo = mkMinion Rare Neutral StampedingKodo 5 3 5 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ Elect $ A $ Minion [OwnedBy opponent, AttackCond LessEqual 2] $ \victim ->
                    Effect $ DestroyMinion victim ]


starfall :: Spell
starfall = mkSpell Rare Druid Starfall 5 $ \this ->
    Choice [
        A $ Minion [] $ \target ->
            Effect $ (this `damages` target) 5,
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                All $ Minions [OwnedBy opponent] $ \victims ->
                    Effect $ ForEach victims $ \victim ->
                        (this `damages` victim) 2 ]


sunwalker :: Minion
sunwalker = mkMinion Rare Neutral Sunwalker 6 4 5 [
    KeywordAbility Taunt,
    KeywordAbility DivineShield ]


taurenWarrior :: Minion
taurenWarrior = mkMinion Common Neutral TaurenWarrior 3 2 3 [
    KeywordAbility Taunt,
    KeywordAbility $ Enrage [] [
        StatsDelta 3 0 ]]


templeEnforcer :: Minion
templeEnforcer = mkMinion Common Priest TempleEnforcer 6 6 6 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion [OwnedBy you, Not this] $ \target ->
                Effect $ Enchant target $ Continuous $ StatsDelta 0 3 ]


twistingNether :: Spell
twistingNether = mkSpell Epic Warlock TwistingNether 8 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ ForEach minions $ \minion ->
            DestroyMinion minion


wisp :: Minion
wisp = mkMinion Common Neutral Wisp 0 1 1 []


wrath :: Spell
wrath = mkSpell Common Druid Wrath 2 $ \this ->
    Choice [
        A $ Minion [] $ \target ->
            Effect $ (this `damages` target) 3,
        A $ Minion [] $ \target ->
            OwnerOf this $ \you ->
                Effect $ Sequence [
                    (this `damages` target) 1,
                    DrawCards you 1 ]]










