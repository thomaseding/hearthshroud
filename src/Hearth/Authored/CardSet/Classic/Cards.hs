{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Hearth.Authored.CardSet.Classic.Cards (
    cards,
) where


--------------------------------------------------------------------------------


import Hearth.Authored.CardSet.Classic.Names hiding (Silence)
import Hearth.Combinator.Authoring
import Hearth.Model.Authoring
import Prelude hiding (sequence)

import qualified Hearth.Authored.CardSet.Classic.Names as Classic


--------------------------------------------------------------------------------


cards :: [Card]
cards = let x = toCard in [
    x abomination,
    x abusiveSergeant,
    x al'AkirTheWindlord,
    x aldorPeacekeeper,
    x amaniBerserker,
    x ancientOfLore,
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


mkMinion :: Rarity -> Class -> ClassicCardName -> [Tribe] -> Mana -> Attack -> Health -> [Ability 'Minion'] -> MinionCard
mkMinion = mkMinion' ClassicCardName


mkSpell :: Rarity -> Class -> ClassicCardName -> Mana -> SpellEffect -> SpellCard
mkSpell = mkSpell' ClassicCardName


mkWeapon :: Rarity -> Class -> ClassicCardName -> Mana -> Attack -> Durability -> [Ability 'Weapon'] -> WeaponCard
mkWeapon = mkWeapon' ClassicCardName


--------------------------------------------------------------------------------


abomination :: MinionCard
abomination = mkMinion Rare Neutral Abomination [] _5 _4 _4 [
    Taunt,
    Deathrattle $ \this ->
        All $ Characters [] $ \victims ->
            Effect $ forEach victims $ \victim ->
                (this `damages` victim) _2 ]


abusiveSergeant :: MinionCard
abusiveSergeant = mkMinion Common Neutral AbusiveSergeant [] _1 _2 _1 [
    Battlecry $ \_ ->
        A $ Minion [] $ \target ->
            Effect $ enchant target $ Until EndOfTurn $ gainAttack _2 ]


amaniBerserker :: MinionCard
amaniBerserker = mkMinion Common Neutral AmaniBerserker [] _2 _2 _3 [
    Enrage [] [
        gainAttack _3 ]]


al'AkirTheWindlord :: MinionCard
al'AkirTheWindlord = mkMinion Legendary Shaman Al'AkirTheWindlord [] _8 _3 _5 [
    Windfury,
    Charge,
    DivineShield,
    Taunt ]


aldorPeacekeeper :: MinionCard
aldorPeacekeeper = mkMinion Rare Paladin AldorPeacekeeper [] _3 _3 _3 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            opponentOf you $ \opponent ->
                A $ Minion [OwnedBy opponent] $ \target ->
                    Effect $ enchant target $ ChangeStat (Left _1) ]


ancientOfLore :: MinionCard
ancientOfLore = mkMinion Epic Druid AncientOfLore [] _7 _5 _5 [
    ChooseOne $ \this -> [
        ownerOf this $ \you ->
            Effect $ DrawCards you 1,
        A $ Character [] $ \character ->
            Effect $ RestoreHealth character _5 ]]


ancientWatcher :: MinionCard
ancientWatcher = mkMinion Rare Neutral AncientWatcher [] _2 _4 _5 [
    Can'tAttack ]


arcaneGolem :: MinionCard
arcaneGolem = mkMinion Rare Neutral ArcaneGolem [] _3 _4 _4 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            opponentOf you $ \opponent ->
                Effect $ GainManaCrystals opponent 1 CrystalFull ]


argentCommander :: MinionCard
argentCommander = mkMinion Rare Neutral ArgentCommander [] _6 _4 _2 [
    Charge,
    DivineShield ]


argentProtector :: MinionCard
argentProtector = mkMinion Common Paladin ArgentProtector [] _2 _2 _2 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            A $ Minion [OwnedBy you] $ \target ->
                Effect $ enchant target $ Grant DivineShield ]


argentSquire :: MinionCard
argentSquire = mkMinion Common Neutral ArgentSquire [] _1 _1 _1 [
    DivineShield ]


armorsmith :: MinionCard
armorsmith = mkMinion Rare Warrior Armorsmith [] _2 _1 _4 [
    observer $ \this ->
        DamageIsDealt $ \victim _ _ ->
            ownerOf this $ \you ->
                Effect $ when (victim `Satisfies` [OwnedBy you, IsMinion]) $ GainArmor you _1 ]


ashbringer :: WeaponCard
ashbringer = uncollectible $ mkWeapon Legendary Paladin Ashbringer _5 _5 _3 []


azureDrake :: MinionCard
azureDrake = mkMinion Rare Neutral AzureDrake [Dragon] _5 _4 _4 [
    SpellDamage 1,
    Battlecry $ \this ->
        ownerOf this $ \you ->
            Effect $ DrawCards you 1 ]


baronGeddon :: MinionCard
baronGeddon = mkMinion Legendary Neutral BaronGeddon [] _7 _7 _5 [
    observer $ \this ->
        EndOfTurnEvent $ \player ->
            ownerOf this $ \you ->
                Effect $ when (player `Satisfies` [Is you]) $ Get $ All $ Characters [Not $ asCharacter this] $ \characters ->
                    Effect $ forEach characters $ \character ->
                        (this `damages` character) _2 ]


battleRage :: SpellCard
battleRage = mkSpell Common Warrior BattleRage _2 $ \this ->
    ownerOf this $ \you ->
        All $ Characters [damaged, OwnedBy you] $ \friendlies ->
            Effect $ forEach friendlies $ \_ ->
                DrawCards you 1


bigGameHunter :: MinionCard
bigGameHunter = mkMinion Epic Neutral BigGameHunter [] _5 _4 _2 [
    Battlecry $ \_ ->
        A $ Minion [withAttack GreaterEqual _7] $ \target ->
            Effect $ destroy target ]


bite :: SpellCard
bite = mkSpell Rare Druid Bite _4 $ \this ->
    ownerOf this $ \you ->
        Effect $ sequence [
            enchant you $ Until EndOfTurn $ gainAttack _4,
            GainArmor you _4 ]


blessedChampion :: SpellCard
blessedChampion = mkSpell Rare Paladin BlessedChampion _5 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ enchant target $ StatsScale _2 _1


blizzard :: SpellCard
blizzard = mkSpell Rare Mage Blizzard _6 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ forEach victims $ \victim -> sequence [
                    (this `damages` victim) _2,
                    freeze victim ]


bloodmageThalnos :: MinionCard
bloodmageThalnos = mkMinion Legendary Neutral BloodmageThalnos [] _2 _1 _1 [
    SpellDamage 1,
    Deathrattle $ \this ->
        ownerOf this $ \you ->
            Effect $ DrawCards you 1 ]


brawl :: SpellCard
brawl = mkSpell Epic Warrior Brawl _5 $ \_ ->
    Effect $ Get $ A $ Minion [] $ \survivor ->
        A $ Minion [Not survivor] $ \someNonSurvivor ->
            All $ Minions [Not survivor] $ \victims ->
                Effect $ sequence [
                    Unreferenced someNonSurvivor, -- This is because Brawl requires at least _2 minions to play.
                    forEach victims $ \victim ->
                        destroy victim ]


circleOfHealing :: SpellCard
circleOfHealing = mkSpell Common Priest CircleOfHealing _0 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ forEach minions $ \minion ->
            RestoreHealth (asCharacter minion) _4


coldlightOracle :: MinionCard
coldlightOracle = mkMinion Rare Neutral ColdlightOracle [Murloc] _3 _2 _2[
    Battlecry $ \_ ->
        All $ Players [] $ \players ->
            Effect $ forEach players $ \player ->
                DrawCards player 2 ]


crazedAlchemist :: MinionCard
crazedAlchemist = mkMinion Rare Neutral CrazedAlchemist [] _2 _2 _2 [
    Battlecry $ \_ ->
        A $ Minion [] $ \target ->
            Effect $ enchant target SwapStats ]


cruelTaskmaster :: MinionCard
cruelTaskmaster = mkMinion Common Warrior CruelTaskmaster [] _2 _2 _2 [
    Battlecry $ \this ->
        A $ Minion [] $ \target ->
            Effect $ sequence [
                (this `damages` target) _1,
                enchant target $ gainAttack _2 ]]


darkIronDwarf :: MinionCard
darkIronDwarf = mkMinion Common Neutral DarkIronDwarf [] _4 _4 _4 [
    Battlecry $ \_ ->
        A $ Minion [] $ \target ->
            Effect $ enchant target $ Until EndOfTurn $ gainAttack _2 ]


direWolfAlpha :: MinionCard
direWolfAlpha = mkMinion Common Neutral DireWolfAlpha [Beast] _2 _2 _2 [
    aura $ \this ->
        EachMinion [AdjacentTo this] $ \minion ->
            Has minion $ gainAttack _1 ]


earthenRingFarseer :: MinionCard
earthenRingFarseer = mkMinion Common Neutral EarthenRingFarseer [] _3 _3 _3 [
    Battlecry $ \_ ->
        A $ Character [] $ \character ->
            Effect $ RestoreHealth character _3 ]


earthShock :: SpellCard
earthShock = mkSpell Common Shaman EarthShock _1 $ \this ->
    A $ Minion [] $ \target ->
        Effect $ sequence [
            Silence target,
            (this `damages` target) _1 ]


emeraldDrake :: MinionCard
emeraldDrake = uncollectible $ mkMinion Free Hunter EmeraldDrake [Dragon] _4 _7 _6 []


equality :: SpellCard
equality = mkSpell Rare Paladin Equality _2 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ forEach minions $ \minion ->
            enchant minion $ ChangeStat (Right _1)


fenCreeper :: MinionCard
fenCreeper = mkMinion Common Neutral FenCreeper [] _5 _3 _6 [
    Taunt ]


flameImp :: MinionCard
flameImp = mkMinion Common Warlock FlameImp [Demon] _1 _3 _2 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            Effect $ (this `damages` you) _3 ]


frostElemental :: MinionCard
frostElemental = mkMinion Common Neutral FrostElemental [] _6 _5 _5 [
    Battlecry $ \_ ->
        A $ Character [] $ \victim ->
            Effect $ Freeze victim ]


gadgetzanAuctioneer :: MinionCard
gadgetzanAuctioneer = mkMinion Rare Neutral GadgetzanAuctioneer [] _6 _4 _4 [
    observer $ \this ->
        SpellIsCast $ \spell ->
            ownerOf this $ \you ->
                Effect $ when (spell `Satisfies` [OwnedBy you]) $ DrawCards you 1 ]


gnoll :: MinionCard
gnoll = uncollectible $ mkMinion Free Neutral Gnoll [] _2 _2 _2 [
    Taunt ]


grommashHellscream :: MinionCard
grommashHellscream = mkMinion Legendary Warrior GrommashHellscream [] _8 _4 _9 [
    Charge,
    Enrage [] [
        gainAttack _6 ]]


gruul :: MinionCard
gruul = mkMinion Legendary Neutral Gruul [] _8 _7 _7 [
    observer $ \this ->
        EndOfTurnEvent $ \_ ->
            Effect $ sequence [
                enchant this $ gainAttack _1,
                enchant this $ GainHealth _1 ]]


hogger :: MinionCard
hogger = mkMinion Legendary Neutral Hogger [] _6 _4 _4 [
    observer $ \this ->
        EndOfTurnEvent $ \player ->
            ownerOf this $ \you ->
                Effect $ when (player `Satisfies` [Is you]) $ (Summon gnoll) $ Rightmost you ]


holyFire :: SpellCard
holyFire = mkSpell Rare Priest HolyFire _6 $ \this ->
    A $ Character [] $ \target ->
        ownerOf this $ \you ->
            Effect $ sequence [
                (this `damages` target) _5,
                RestoreHealth (asCharacter you) _5 ]


infernal :: MinionCard
infernal = uncollectible $ mkMinion Common Warlock Infernal [Demon] _6 _6 _6 []


injuredBlademaster :: MinionCard
injuredBlademaster = mkMinion Rare Neutral InjuredBlademaster [] _3 _4 _7 [
    Battlecry $ \this ->
        Effect $ (this `damages` this) _4 ]


innerRage :: SpellCard
innerRage = mkSpell Common Warrior InnerRage _0 $ \this ->
    A $ Minion [] $ \target ->
        Effect $ sequence [
            (this `damages` target) _1,
            enchant target $ gainAttack _2 ]


ironbeakOwl :: MinionCard
ironbeakOwl = mkMinion Common Neutral IronbeakOwl [Beast] _3 _2 _1 [
    Battlecry $ \_ ->
        A $ Minion [] $ \target ->
            Effect $ Silence target ]


layOnHands :: SpellCard
layOnHands = mkSpell Epic Paladin LayOnHands _8 $ \this ->
    A $ Character [] $ \target ->
        ownerOf this $ \you ->
            Effect $ sequence [
                RestoreHealth target _6,
                DrawCards you 3 ]


leperGnome :: MinionCard
leperGnome = mkMinion Common Neutral LeperGnome [] _1 _1 _1 [
    Deathrattle $ \this ->
        ownerOf this $ \you ->
            opponentOf you $ \opponent ->
                Effect $ (this `damages` opponent) _2 ]


lootHoarder :: MinionCard
lootHoarder = mkMinion Common Neutral LootHoarder [] _2 _2 _1 [
    Deathrattle $ \this ->
        ownerOf this $ \you ->
            Effect $ DrawCards you 1 ]


malygos :: MinionCard
malygos = mkMinion Legendary Neutral Malygos [Dragon] _9 _4 _12 [
    SpellDamage 5 ]


markOfNature :: SpellCard
markOfNature = mkSpell Common Druid MarkOfNature _3 $ \_ ->
    ChooseOne' [
        A $ Minion [] $ \target ->
            Effect $ enchant target $ gainAttack _4,
        A $ Minion [] $ \target ->
            Effect $ sequence [
                enchant target $ GainHealth _4,
                enchant target $ Grant Taunt ]]


massDispel :: SpellCard
massDispel = mkSpell Rare Priest MassDispel _4 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ sequence [
                    forEach victims $ \victim ->
                        Silence victim,
                    DrawCards you 1 ]


mogu'shanWarden :: MinionCard
mogu'shanWarden = mkMinion Common Neutral Mogu'shanWarden [] _4 _1 _7 [
    Taunt ]


naturalize :: SpellCard
naturalize = mkSpell Common Druid Naturalize _1 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            A $ Minion [] $ \target ->
                Effect $ sequence [
                    destroy target,
                    DrawCards opponent 2 ]


nourish :: SpellCard
nourish = mkSpell Rare Druid Nourish _5 $ \this ->
    ownerOf this $ \you ->
        ChooseOne' [
            Effect $ GainManaCrystals you 2 CrystalFull,
            Effect $ DrawCards you 3 ]


pitLord :: MinionCard
pitLord = mkMinion Epic Warlock PitLord [Demon] _4 _5 _6 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            Effect $ (this `damages` you) _5 ]


priestessOfElune :: MinionCard
priestessOfElune = mkMinion Common Neutral PriestessOfElune [] _6 _5 _4 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            Effect $ RestoreHealth (asCharacter you) _4 ]


pyroblast :: SpellCard
pyroblast = mkSpell Epic Mage Pyroblast _10 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) _10


ragnarosTheFirelord :: MinionCard
ragnarosTheFirelord = mkMinion Legendary Neutral RagnarosTheFirelord [] _8 _8 _8 [
    Can'tAttack,
    observer $ \this ->
        EndOfTurnEvent $ \player ->
            ownerOf this $ \you ->
                opponentOf you $ \opponent ->
                    Effect $ when (player `Satisfies` [Is you]) $ Get $ A $ Character [OwnedBy opponent] $ \enemy ->
                        Effect $ (this `damages` enemy) _8 ]


rampage :: SpellCard
rampage = mkSpell Common Warrior Rampage _2 $ \_ ->
    A $ Minion [damaged] $ \target ->
        Effect $ sequence [
            enchant target $ gainAttack _3,
            enchant target $ GainHealth _3 ]


scarletCrusader :: MinionCard
scarletCrusader = mkMinion Common Neutral ScarletCrusader [] _3 _3 _1 [
    DivineShield ]


shieldbearer :: MinionCard
shieldbearer = mkMinion Common Neutral Shieldbearer [] _1 _0 _4 [
    Taunt ]


silence :: SpellCard
silence = mkSpell Common Priest Classic.Silence _0 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Silence target


silvermoonGuardian :: MinionCard
silvermoonGuardian = mkMinion Common Neutral SilvermoonGuardian [] _4 _3 _3 [
    DivineShield ]


siphonSoul :: SpellCard
siphonSoul = mkSpell Rare Warlock SiphonSoul _6 $ \this ->
    A $ Minion [] $ \target ->
        ownerOf this $ \you ->
            Effect $ sequence [
                destroy target,
                RestoreHealth (asCharacter you) _3 ]


soulOfTheForest :: SpellCard
soulOfTheForest = mkSpell Common Druid SoulOfTheForest _4 $ \this ->
    ownerOf this $ \you ->
        All $ Minions [OwnedBy you] $ \minions ->
            Effect $ forEach minions $ \minion ->
                enchant minion $ Grant $ Deathrattle $ \this' ->
                    ownerOf this' $ \owner ->
                        Effect $ (Summon treant_soulOfTheForest) $ Rightmost owner


spellbreaker :: MinionCard
spellbreaker = mkMinion Common Neutral Spellbreaker [] _4 _4 _3 [
    Battlecry $ \_ ->
        A $ Minion [] $ \target ->
            Effect $ Silence target ]


stampedingKodo :: MinionCard
stampedingKodo = mkMinion Rare Neutral StampedingKodo [Beast] _5 _3 _5 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            opponentOf you $ \opponent ->
                Effect $ Get $ A $ Minion [OwnedBy opponent, withAttack LessEqual _2] $ \victim ->
                    Effect $ destroy victim ]


starfall :: SpellCard
starfall = mkSpell Rare Druid Starfall _5 $ \this ->
    ChooseOne' [
        A $ Minion [] $ \target ->
            Effect $ (this `damages` target) _5,
        ownerOf this $ \you ->
            opponentOf you $ \opponent ->
                All $ Minions [OwnedBy opponent] $ \victims ->
                    Effect $ forEach victims $ \victim ->
                        (this `damages` victim) _2 ]


sunwalker :: MinionCard
sunwalker = mkMinion Rare Neutral Sunwalker [] _6 _4 _5 [
    Taunt,
    DivineShield ]


taurenWarrior :: MinionCard
taurenWarrior = mkMinion Common Neutral TaurenWarrior [] _3 _2 _3 [
    Taunt,
    Enrage [] [
        gainAttack _3 ]]


templeEnforcer :: MinionCard
templeEnforcer = mkMinion Common Priest TempleEnforcer [] _6 _6 _6 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            A $ Minion [OwnedBy you] $ \target ->
                Effect $ enchant target $ GainHealth _3 ]


tirionFordring :: MinionCard
tirionFordring = mkMinion Legendary Paladin TirionFordring [] _8 _6 _6 [
    DivineShield,
    Taunt,
    Deathrattle $ \this ->
        ownerOf this $ \you ->
            Effect $ EquipWeapon you ashbringer ]


treant_soulOfTheForest :: MinionCard
treant_soulOfTheForest = uncollectible $ mkMinion Free Druid Treant_SoulOfTheForest [] _1 _2 _2 []


twistingNether :: SpellCard
twistingNether = mkSpell Epic Warlock TwistingNether _8 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ forEach minions $ \minion ->
            destroy minion


windfuryHarpy :: MinionCard
windfuryHarpy = mkMinion Common Neutral WindfuryHarpy [] _6 _4 _5 [
    Windfury ]


wisp :: MinionCard
wisp = mkMinion Common Neutral Wisp [] _0 _1 _1 []


wrath :: SpellCard
wrath = mkSpell Common Druid Wrath _2 $ \this ->
    ChooseOne' [
        A $ Minion [] $ \target ->
            Effect $ (this `damages` target) _3,
        A $ Minion [] $ \target ->
            ownerOf this $ \you ->
                Effect $ sequence [
                    (this `damages` target) _1,
                    DrawCards you 1 ]]










