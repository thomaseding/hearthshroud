{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Hearth.CardSet.Classic.Cards (
    cards,
) where


--------------------------------------------------------------------------------


import Hearth.Authoring.Combinators
import Hearth.CardName
import Hearth.CardSet.Classic.Names hiding (Silence)
import qualified Hearth.CardSet.Classic.Names as Classic
import Hearth.Model


--------------------------------------------------------------------------------


cards :: (UserConstraint k) => [Card k]
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


mkMinion :: (UserConstraint k) => Rarity -> Class -> ClassicCardName -> [MinionType] -> Mana -> Attack -> Health -> [Ability k Minion] -> MinionCard k
mkMinion = mkMinion' ClassicCardName


mkSpell :: (UserConstraint k) => Rarity -> Class -> ClassicCardName -> Mana -> SpellEffect k -> SpellCard k
mkSpell = mkSpell' ClassicCardName


--------------------------------------------------------------------------------


abomination :: (UserConstraint k) => MinionCard k
abomination = mkMinion Rare Neutral Abomination [] 5 4 4 [
    Taunt,
    Deathrattle $ \this ->
        All $ Characters [Not (MinionCharacter this)] $ \victims ->
            Effect $ ForEach victims $ \victim ->
                (this `damages` victim) 2 ]


abusiveSergeant :: (UserConstraint k) => MinionCard k
abusiveSergeant = mkMinion Common Neutral AbusiveSergeant [] 1 2 1 [
    Battlecry $ \this ->
        A $ Minion [Not this] $ \target ->
            Effect $ Enchant target $ Limited $ Until EndOfTurn $ statsDelta 2 0 ]


amaniBerserker :: (UserConstraint k) => MinionCard k
amaniBerserker = mkMinion Common Neutral AmaniBerserker [] 2 2 3 [
    Enrage [] [
        statsDelta 3 0 ]]


aldorPeacekeeper :: (UserConstraint k) => MinionCard k
aldorPeacekeeper = mkMinion Rare Paladin AldorPeacekeeper [] 3 3 3 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                A $ Minion [OwnedBy opponent] $ \target ->
                    Effect $ Enchant target $ Continuous $ ChangeStat (Left 1) ]


arcaneGolem :: (UserConstraint k) => MinionCard k
arcaneGolem = mkMinion Rare Neutral ArcaneGolem [] 3 4 2 [
    Charge,
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ GainManaCrystals opponent 1 CrystalFull ]


argentCommander :: (UserConstraint k) => MinionCard k
argentCommander = mkMinion Rare Neutral ArgentCommander [] 6 4 2 [
    Charge,
    DivineShield ]


argentProtector :: (UserConstraint k) => MinionCard k
argentProtector = mkMinion Common Paladin ArgentProtector [] 2 2 2 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion [OwnedBy you, Not this] $ \target ->
                Effect $ Enchant target $ Continuous $ Grant DivineShield ]


argentSquire :: (UserConstraint k) => MinionCard k
argentSquire = mkMinion Common Neutral ArgentSquire [] 1 1 1 [
    DivineShield ]


armorsmith :: (UserConstraint k) => MinionCard k
armorsmith = mkMinion Rare Warrior Armorsmith [] 2 1 4 [
    Whenever $ \this ->
        DamageIsDealt $ \victim _ _ ->
            OwnerOf this $ \you ->
                Effect $ when (victim `Satisfies` [OwnedBy you, IsMinion]) $ GainArmor you 1 ]


battleRage :: (UserConstraint k) => SpellCard k
battleRage = mkSpell Common Warrior BattleRage 2 $ \this ->
    OwnerOf this $ \you ->
        All $ Characters [Damaged, OwnedBy you] $ \friendlies ->
            Effect $ ForEach friendlies $ \_ ->
                DrawCards you 1


bigGameHunter :: (UserConstraint k) => MinionCard k
bigGameHunter = mkMinion Epic Neutral BigGameHunter [] 3 4 2 [
    Battlecry $ \this ->
        A $ Minion [Not this, RequireMinion (WithAttack GreaterEqual 7)] $ \target ->
            Effect $ DestroyMinion target ]


blessedChampion :: (UserConstraint k) => SpellCard k
blessedChampion = mkSpell Rare Paladin BlessedChampion 5 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ StatsScale 2 1


brawl :: (UserConstraint k) => SpellCard k
brawl = mkSpell Epic Warrior Brawl 5 $ \_ ->
    Effect $ Elect $ A $ Minion [] $ \survivor ->
        A $ Minion [Not survivor] $ \someNonSurvivor ->
            All $ Minions [Not survivor] $ \victims ->
                Effect $ Sequence [
                    Unreferenced someNonSurvivor, -- This is because Brawl requires at least 2 minions to play.
                    ForEach victims $ \victim ->
                        DestroyMinion victim ]


circleOfHealing :: (UserConstraint k) => SpellCard k
circleOfHealing = mkSpell Common Priest CircleOfHealing 0 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ ForEach minions $ \minion ->
            RestoreHealth (MinionCharacter minion) 4


coldlightOracle :: (UserConstraint k) => MinionCard k
coldlightOracle = mkMinion Rare Neutral ColdlightOracle [Murloc] 3 2 2[
    Battlecry $ \_ ->
        All $ Players [] $ \players ->
            Effect $ ForEach players $ \player ->
                DrawCards player 2 ]


crazedAlchemist :: (UserConstraint k) => MinionCard k
crazedAlchemist = mkMinion Rare Neutral CrazedAlchemist [] 2 2 2 [
    Battlecry $ \this ->
        A $ Minion [Not this] $ \target ->
            Effect $ Enchant target $ Continuous SwapStats ]


cruelTaskmaster :: (UserConstraint k) => MinionCard k
cruelTaskmaster = mkMinion Common Warrior CruelTaskmaster [] 2 2 2 [
    Battlecry $ \this ->
        A $ Minion [Not this] $ \target ->
            Effect $ Sequence [
                (this `damages` target) 1,
                Enchant target $ Continuous $ statsDelta 2 0 ]]


direWolfAlpha :: (UserConstraint k) => MinionCard k
direWolfAlpha = mkMinion Common Neutral DireWolfAlpha [Beast] 2 2 2 [
    Aura $ \this ->
        EachMinion [AdjacentTo this] $ \minion ->
            Has minion $ statsDelta 1 0 ]


earthenRingFarseer :: (UserConstraint k) => MinionCard k
earthenRingFarseer = mkMinion Common Neutral EarthenRingFarseer [] 3 3 3 [
    Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \character ->
            Effect $ RestoreHealth character 3 ]


earthShock :: (UserConstraint k) => SpellCard k
earthShock = mkSpell Common Shaman EarthShock 1 $ \this ->
    A $ Minion [] $ \target ->
        Effect $ Sequence [
            Silence target,
            (this `damages` target) 1 ]


equality :: (UserConstraint k) => SpellCard k
equality = mkSpell Rare Paladin Equality 2 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ ForEach minions $ \minion ->
            Enchant minion $ Continuous $ ChangeStat (Right 1)


fenCreeper :: (UserConstraint k) => MinionCard k
fenCreeper = mkMinion Common Neutral FenCreeper [] 5 3 6 [
    Taunt ]


flameImp :: (UserConstraint k) => MinionCard k
flameImp = mkMinion Common Warlock FlameImp [Demon] 1 3 2 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ (this `damages` you) 3 ]


gadgetzanAuctioneer :: (UserConstraint k) => MinionCard k
gadgetzanAuctioneer = mkMinion Rare Neutral GadgetzanAuctioneer [] 6 4 4 [
    Whenever $ \this ->
        SpellIsCast $ \spell ->
            OwnerOf this $ \you ->
                Effect $ when (spell `Satisfies` [OwnedBy you]) $ DrawCards you 1 ]


grommashHellscream :: (UserConstraint k) => MinionCard k
grommashHellscream = mkMinion Legendary Warrior GrommashHellscream [] 8 4 9 [
    Charge,
    Enrage [] [
        statsDelta 6 0 ]]


holyFire :: (UserConstraint k) => SpellCard k
holyFire = mkSpell Rare Priest HolyFire 6 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 5,
                RestoreHealth (PlayerCharacter you) 5 ]


injuredBlademaster :: (UserConstraint k) => MinionCard k
injuredBlademaster = mkMinion Rare Neutral InjuredBlademaster [] 3 4 7 [
    Battlecry $ \this ->
        Effect $ (this `damages` this) 4 ]


innerRage :: (UserConstraint k) => SpellCard k
innerRage = mkSpell Common Warrior InnerRage 0 $ \this ->
    A $ Minion [] $ \target ->
        Effect $ Sequence [
            (this `damages` target) 1,
            Enchant target $ Continuous $ statsDelta 2 0 ]


ironbeakOwl :: (UserConstraint k) => MinionCard k
ironbeakOwl = mkMinion Common Neutral IronbeakOwl [Beast] 2 2 1 [
    Battlecry $ \this ->
        A $ Minion [Not this] $ \target ->
            Effect $ Silence target ]


layOnHands :: (UserConstraint k) => SpellCard k
layOnHands = mkSpell Epic Paladin LayOnHands 8 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                RestoreHealth target 6,
                DrawCards you 3 ]


leperGnome :: (UserConstraint k) => MinionCard k
leperGnome = mkMinion Common Neutral LeperGnome [] 1 2 1 [
    Deathrattle $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ (this `damages` opponent) 2 ]


lootHoarder :: (UserConstraint k) => MinionCard k
lootHoarder = mkMinion Common Neutral LootHoarder [] 2 2 1 [
    Deathrattle $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


markOfNature :: (UserConstraint k) => SpellCard k
markOfNature = mkSpell Common Druid MarkOfNature 3 $ \_ ->
    Choice [
        A $ Minion [] $ \target ->
            Effect $ Enchant target $ Continuous $ statsDelta 4 0,
        A $ Minion [] $ \target ->
            Effect $ Sequence [
                Enchant target $ Continuous $ statsDelta 0 4,
                Enchant target $ Continuous $ Grant Taunt ]]


massDispel :: (UserConstraint k) => SpellCard k
massDispel = mkSpell Rare Priest MassDispel 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ Sequence [
                    ForEach victims $ \victim ->
                        Silence victim,
                    DrawCards you 1 ]


mogu'shanWarden :: (UserConstraint k) => MinionCard k
mogu'shanWarden = mkMinion Common Neutral Mogu'shanWarden [] 4 1 7 [
    Taunt ]


naturalize :: (UserConstraint k) => SpellCard k
naturalize = mkSpell Common Druid Naturalize 1 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            A $ Minion [] $ \target ->
                Effect $ Sequence [
                    DestroyMinion target,
                    DrawCards opponent 2 ]


nourish :: (UserConstraint k) => SpellCard k
nourish = mkSpell Rare Druid Nourish 5 $ \this ->
    OwnerOf this $ \you ->
        Choice [
            Effect $ GainManaCrystals you 2 CrystalFull,
            Effect $ DrawCards you 3 ]


pitLord :: (UserConstraint k) => MinionCard k
pitLord = mkMinion Epic Warlock PitLord [Demon] 4 5 6 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ (this `damages` you) 5 ]


priestessOfElune :: (UserConstraint k) => MinionCard k
priestessOfElune = mkMinion Common Neutral PriestessOfElune [] 6 5 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ RestoreHealth (PlayerCharacter you) 4 ]


pyroblast :: (UserConstraint k) => SpellCard k
pyroblast = mkSpell Epic Mage Pyroblast 10 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) 10


rampage :: (UserConstraint k) => SpellCard k
rampage = mkSpell Common Warrior Rampage 2 $ \_ ->
    A $ Minion [RequireMinion Damaged] $ \target ->
        Effect $ Enchant target $ Continuous $ statsDelta 3 3


scarletCrusader :: (UserConstraint k) => MinionCard k
scarletCrusader = mkMinion Common Neutral ScarletCrusader [] 3 3 1 [
    DivineShield ]


shieldbearer :: (UserConstraint k) => MinionCard k
shieldbearer = mkMinion Common Neutral Shieldbearer [] 1 0 4 [
    Taunt ]


silence :: (UserConstraint k) => SpellCard k
silence = mkSpell Common Priest Classic.Silence 0 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Silence target


silvermoonGuardian :: (UserConstraint k) => MinionCard k
silvermoonGuardian = mkMinion Common Neutral SilvermoonGuardian [] 4 3 3 [
    DivineShield ]


siphonSoul :: (UserConstraint k) => SpellCard k
siphonSoul = mkSpell Rare Warlock SiphonSoul 6 $ \this ->
    A $ Minion [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                DestroyMinion target,
                RestoreHealth (PlayerCharacter you) 3 ]


spellbreaker :: (UserConstraint k) => MinionCard k
spellbreaker = mkMinion Common Neutral Spellbreaker [] 4 4 3 [
    Battlecry $ \this ->
        A $ Minion [Not this] $ \target ->
            Effect $ Silence target ]


stampedingKodo :: (UserConstraint k) => MinionCard k
stampedingKodo = mkMinion Rare Neutral StampedingKodo [Beast] 5 3 5 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ Elect $ A $ Minion [OwnedBy opponent, RequireMinion (WithAttack LessEqual 2)] $ \victim ->
                    Effect $ DestroyMinion victim ]


starfall :: (UserConstraint k) => SpellCard k
starfall = mkSpell Rare Druid Starfall 5 $ \this ->
    Choice [
        A $ Minion [] $ \target ->
            Effect $ (this `damages` target) 5,
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                All $ Minions [OwnedBy opponent] $ \victims ->
                    Effect $ ForEach victims $ \victim ->
                        (this `damages` victim) 2 ]


sunwalker :: (UserConstraint k) => MinionCard k
sunwalker = mkMinion Rare Neutral Sunwalker [] 6 4 5 [
    Taunt,
    DivineShield ]


taurenWarrior :: (UserConstraint k) => MinionCard k
taurenWarrior = mkMinion Common Neutral TaurenWarrior [] 3 2 3 [
    Taunt,
    Enrage [] [
        statsDelta 3 0 ]]


templeEnforcer :: (UserConstraint k) => MinionCard k
templeEnforcer = mkMinion Common Priest TempleEnforcer [] 6 6 6 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion [OwnedBy you, Not this] $ \target ->
                Effect $ Enchant target $ Continuous $ statsDelta 0 3 ]


twistingNether :: (UserConstraint k) => SpellCard k
twistingNether = mkSpell Epic Warlock TwistingNether 8 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ ForEach minions $ \minion ->
            DestroyMinion minion


wisp :: (UserConstraint k) => MinionCard k
wisp = mkMinion Common Neutral Wisp [] 0 1 1 []


wrath :: (UserConstraint k) => SpellCard k
wrath = mkSpell Common Druid Wrath 2 $ \this ->
    Choice [
        A $ Minion [] $ \target ->
            Effect $ (this `damages` target) 3,
        A $ Minion [] $ \target ->
            OwnerOf this $ \you ->
                Effect $ Sequence [
                    (this `damages` target) 1,
                    DrawCards you 1 ]]










