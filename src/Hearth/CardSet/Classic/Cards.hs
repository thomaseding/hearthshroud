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
    x al'AkirTheWindlord,
    x aldorPeacekeeper,
    x amaniBerserker,
    x ancientWatcher,
    x arcaneGolem,
    x argentCommander,
    x argentProtector,
    x argentSquire,
    x armorsmith,
    x ashbringer,
    x azureDrake,
    x baronGeddon,
    x battleRage,
    x bigGameHunter,
    x bite,
    x blessedChampion,
    x blizzard,
    x bloodmageThalnos,
    x brawl,
    x circleOfHealing,
    x coldlightOracle,
    x crazedAlchemist,
    x cruelTaskmaster,
    x darkIronDwarf,
    x direWolfAlpha,
    x earthenRingFarseer,
    x earthShock,
    x emeraldDrake,
    x equality,
    x fenCreeper,
    x flameImp,
    x frostElemental,
    x gadgetzanAuctioneer,
    x gnoll,
    x grommashHellscream,
    x gruul,
    x holyFire,
    x hogger,
    x infernal,
    x injuredBlademaster,
    x innerRage,
    x ironbeakOwl,
    x layOnHands,
    x leperGnome,
    x lootHoarder,
    x malygos,
    x markOfNature,
    x massDispel,
    x mogu'shanWarden,
    x naturalize,
    x nourish,
    x pitLord,
    x priestessOfElune,
    x pyroblast,
    x ragnarosTheFirelord,
    x rampage,
    x scarletCrusader,
    x shieldbearer,
    x silence,
    x silvermoonGuardian,
    x siphonSoul,
    x soulOfTheForest,
    x spellbreaker,
    x stampedingKodo,
    x starfall,
    x sunwalker,
    x taurenWarrior,
    x templeEnforcer,
    x tirionFordring,
    x treant_soulOfTheForest,
    x twistingNether,
    x windfuryHarpy,
    x wisp,
    x wrath ]


--------------------------------------------------------------------------------


mkMinion :: (UserConstraint k) => Rarity -> Class -> ClassicCardName -> [MinionType] -> Mana -> Attack -> Health -> [Ability k Minion] -> MinionCard k
mkMinion = mkMinion' ClassicCardName


mkSpell :: (UserConstraint k) => Rarity -> Class -> ClassicCardName -> Mana -> SpellEffect k -> SpellCard k
mkSpell = mkSpell' ClassicCardName


mkWeapon :: (UserConstraint k) => Rarity -> Class -> ClassicCardName -> Mana -> Attack -> Durability -> [Ability k Weapon] -> WeaponCard k
mkWeapon = mkWeapon' ClassicCardName


--------------------------------------------------------------------------------


abomination :: (UserConstraint k) => MinionCard k
abomination = mkMinion Rare Neutral Abomination [] 5 4 4 [
    Taunt,
    Deathrattle $ \this ->
        All $ Characters [] $ \victims ->
            Effect $ ForEach victims $ \victim ->
                (this `damages` victim) 2 ]


abusiveSergeant :: (UserConstraint k) => MinionCard k
abusiveSergeant = mkMinion Common Neutral AbusiveSergeant [] 1 2 1 [
    Battlecry $ \_ ->
        A $ Minion [] $ \target ->
            Effect $ Enchant target $ Limited $ Until EndOfTurn $ statsDelta 2 0 ]


amaniBerserker :: (UserConstraint k) => MinionCard k
amaniBerserker = mkMinion Common Neutral AmaniBerserker [] 2 2 3 [
    Enrage [] [
        statsDelta 3 0 ]]


al'AkirTheWindlord :: (UserConstraint k) => MinionCard k
al'AkirTheWindlord = mkMinion Legendary Shaman Al'AkirTheWindlord [] 8 3 5 [
    Windfury,
    Charge,
    DivineShield,
    Taunt ]


aldorPeacekeeper :: (UserConstraint k) => MinionCard k
aldorPeacekeeper = mkMinion Rare Paladin AldorPeacekeeper [] 3 3 3 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                A $ Minion [OwnedBy opponent] $ \target ->
                    Effect $ Enchant target $ Continuous $ ChangeStat (Left 1) ]


ancientWatcher :: (UserConstraint k) => MinionCard k
ancientWatcher = mkMinion Rare Neutral AncientWatcher [] 2 4 5 [
    Can'tAttack ]


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
            A $ Minion [OwnedBy you] $ \target ->
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


ashbringer :: (UserConstraint k) => WeaponCard k
ashbringer = uncollectible $ mkWeapon Legendary Paladin Ashbringer 5 5 3 []


azureDrake :: (UserConstraint k) => MinionCard k
azureDrake = mkMinion Rare Neutral AzureDrake [Dragon] 5 4 4 [
    SpellDamage 1,
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


baronGeddon :: (UserConstraint k) => MinionCard k
baronGeddon = mkMinion Legendary Neutral BaronGeddon [] 7 7 5 [
    Whenever $ \this ->
        EndOfTurnEvent $ \player ->
            OwnerOf this $ \you ->
                Effect $ when (player `Satisfies` [Is you]) $ Elect $ All $ Characters [Not $ MinionCharacter this] $ \characters ->
                    Effect $ ForEach characters $ \character ->
                        (this `damages` character) 2 ]


battleRage :: (UserConstraint k) => SpellCard k
battleRage = mkSpell Common Warrior BattleRage 2 $ \this ->
    OwnerOf this $ \you ->
        All $ Characters [Damaged, OwnedBy you] $ \friendlies ->
            Effect $ ForEach friendlies $ \_ ->
                DrawCards you 1


bigGameHunter :: (UserConstraint k) => MinionCard k
bigGameHunter = mkMinion Epic Neutral BigGameHunter [] 3 4 2 [
    Battlecry $ \_ ->
        A $ Minion [RequireMinion (WithAttack GreaterEqual 7)] $ \target ->
            Effect $ DestroyMinion target ]


bite :: (UserConstraint k) => SpellCard k
bite = mkSpell Rare Druid Bite 4 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Sequence [
            Enchant you $ Limited $ Until EndOfTurn $ statsDelta 4 0,
            GainArmor you 4 ]


blessedChampion :: (UserConstraint k) => SpellCard k
blessedChampion = mkSpell Rare Paladin BlessedChampion 5 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ StatsScale 2 1


blizzard :: (UserConstraint k) => SpellCard k
blizzard = mkSpell Rare Mage Blizzard 6 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ ForEach victims $ \victim -> Sequence [
                    (this `damages` victim) 2,
                    freeze victim ]


bloodmageThalnos :: (UserConstraint k) => MinionCard k
bloodmageThalnos = mkMinion Legendary Neutral BloodmageThalnos [] 2 1 1 [
    SpellDamage 1,
    Deathrattle $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


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
    Battlecry $ \_ ->
        A $ Minion [] $ \target ->
            Effect $ Enchant target $ Continuous SwapStats ]


cruelTaskmaster :: (UserConstraint k) => MinionCard k
cruelTaskmaster = mkMinion Common Warrior CruelTaskmaster [] 2 2 2 [
    Battlecry $ \this ->
        A $ Minion [] $ \target ->
            Effect $ Sequence [
                (this `damages` target) 1,
                Enchant target $ Continuous $ statsDelta 2 0 ]]


darkIronDwarf :: (UserConstraint k) => MinionCard k
darkIronDwarf = mkMinion Common Neutral DarkIronDwarf [] 4 4 4 [
    Battlecry $ \_ ->
        A $ Minion [] $ \target ->
            Effect $ Enchant target $ Limited $ Until EndOfTurn $ statsDelta 2 0 ]


direWolfAlpha :: (UserConstraint k) => MinionCard k
direWolfAlpha = mkMinion Common Neutral DireWolfAlpha [Beast] 2 2 2 [
    Aura $ \this ->
        EachMinion [AdjacentTo this] $ \minion ->
            Has minion $ statsDelta 1 0 ]


earthenRingFarseer :: (UserConstraint k) => MinionCard k
earthenRingFarseer = mkMinion Common Neutral EarthenRingFarseer [] 3 3 3 [
    Battlecry $ \_ ->
        A $ Character [] $ \character ->
            Effect $ RestoreHealth character 3 ]


earthShock :: (UserConstraint k) => SpellCard k
earthShock = mkSpell Common Shaman EarthShock 1 $ \this ->
    A $ Minion [] $ \target ->
        Effect $ Sequence [
            Silence target,
            (this `damages` target) 1 ]


emeraldDrake :: (UserConstraint k) => MinionCard k
emeraldDrake = uncollectible $ mkMinion Free Hunter EmeraldDrake [Dragon] 4 7 6 []


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


frostElemental :: (UserConstraint k) => MinionCard k
frostElemental = mkMinion Common Neutral FrostElemental [] 6 5 5 [
    Battlecry $ \_ ->
        A $ Character [] $ \victim ->
            Effect $ Freeze victim ]


gadgetzanAuctioneer :: (UserConstraint k) => MinionCard k
gadgetzanAuctioneer = mkMinion Rare Neutral GadgetzanAuctioneer [] 6 4 4 [
    Whenever $ \this ->
        SpellIsCast $ \spell ->
            OwnerOf this $ \you ->
                Effect $ when (spell `Satisfies` [OwnedBy you]) $ DrawCards you 1 ]


gnoll :: (UserConstraint k) => MinionCard k
gnoll = uncollectible $ mkMinion Free Neutral Gnoll [] 2 2 2 [
    Taunt ]


grommashHellscream :: (UserConstraint k) => MinionCard k
grommashHellscream = mkMinion Legendary Warrior GrommashHellscream [] 8 4 9 [
    Charge,
    Enrage [] [
        statsDelta 6 0 ]]


gruul :: (UserConstraint k) => MinionCard k
gruul = mkMinion Legendary Neutral Gruul [] 8 7 7 [
    Whenever $ \this ->
        EndOfTurnEvent $ \_ ->
            Effect $ Enchant this $ Continuous $ statsDelta 1 1 ]


hogger :: (UserConstraint k) => MinionCard k
hogger = mkMinion Legendary Neutral Hogger [] 6 4 4 [
    Whenever $ \this ->
        EndOfTurnEvent $ \player ->
            OwnerOf this $ \you ->
                Effect $ when (player `Satisfies` [Is you]) $ (Summon gnoll) $ Rightmost you ]


holyFire :: (UserConstraint k) => SpellCard k
holyFire = mkSpell Rare Priest HolyFire 6 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 5,
                RestoreHealth (PlayerCharacter you) 5 ]


infernal :: (UserConstraint k) => MinionCard k
infernal = uncollectible $ mkMinion Common Warlock Infernal [Demon] 6 6 6 []


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
    Battlecry $ \_ ->
        A $ Minion [] $ \target ->
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


malygos :: (UserConstraint k) => MinionCard k
malygos = mkMinion Legendary Neutral Malygos [Dragon] 9 4 12 [
    SpellDamage 5 ]


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


ragnarosTheFirelord :: (UserConstraint k) => MinionCard k
ragnarosTheFirelord = mkMinion Legendary Neutral RagnarosTheFirelord [] 8 8 8 [
    Can'tAttack,
    Whenever $ \this ->
        EndOfTurnEvent $ \player ->
            OwnerOf this $ \you ->
                OpponentOf you $ \opponent ->
                    Effect $ when (player `Satisfies` [Is you]) $ Elect $ A $ Character [OwnedBy opponent] $ \enemy ->
                        Effect $ (this `damages` enemy) 8 ]


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


soulOfTheForest :: (UserConstraint k) => SpellCard k
soulOfTheForest = mkSpell Common Druid SoulOfTheForest 4 $ \this ->
    OwnerOf this $ \you ->
        All $ Minions [OwnedBy you] $ \minions ->
            Effect $ ForEach minions $ \minion ->
                Enchant minion $ Continuous $ Grant $ Deathrattle $ \this' ->
                    OwnerOf this' $ \owner ->
                        Effect $ (Summon treant_soulOfTheForest) $ Rightmost owner


spellbreaker :: (UserConstraint k) => MinionCard k
spellbreaker = mkMinion Common Neutral Spellbreaker [] 4 4 3 [
    Battlecry $ \_ ->
        A $ Minion [] $ \target ->
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
            A $ Minion [OwnedBy you] $ \target ->
                Effect $ Enchant target $ Continuous $ statsDelta 0 3 ]


tirionFordring :: (UserConstraint k) => MinionCard k
tirionFordring = mkMinion Legendary Paladin TirionFordring [] 8 6 6 [
    DivineShield,
    Taunt,
    Deathrattle $ \this ->
        OwnerOf this $ \you ->
            Effect $ EquipWeapon you ashbringer ]


treant_soulOfTheForest :: (UserConstraint k) => MinionCard k
treant_soulOfTheForest = uncollectible $ mkMinion Free Druid Treant_SoulOfTheForest [] 1 2 2 []


twistingNether :: (UserConstraint k) => SpellCard k
twistingNether = mkSpell Epic Warlock TwistingNether 8 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ ForEach minions $ \minion ->
            DestroyMinion minion


windfuryHarpy :: (UserConstraint k) => MinionCard k
windfuryHarpy = mkMinion Common Neutral WindfuryHarpy [] 6 4 5 [
    Windfury ]


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










