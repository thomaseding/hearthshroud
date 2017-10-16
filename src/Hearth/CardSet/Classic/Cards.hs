{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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


mkMinion :: Rarity -> Class -> ClassicCardName -> [MinionType] -> Mana -> Attack -> Health -> [Ability Minion] -> MinionCard
mkMinion = mkMinion' ClassicCardName


mkSpell :: Rarity -> Class -> ClassicCardName -> Mana -> SpellEffect -> SpellCard
mkSpell = mkSpell' ClassicCardName


mkWeapon :: Rarity -> Class -> ClassicCardName -> Mana -> Attack -> Durability -> [Ability Weapon] -> WeaponCard
mkWeapon = mkWeapon' ClassicCardName


--------------------------------------------------------------------------------


abomination :: MinionCard
abomination = mkMinion Rare Neutral Abomination [] 5 4 4 [
    Taunt,
    Deathrattle $ \this ->
        All $ Characters [] $ \victims ->
            Effect $ ForEachCharacter victims $ \victim ->
                (this `damages` victim) 2 ]


abusiveSergeant :: MinionCard
abusiveSergeant = mkMinion Common Neutral AbusiveSergeant [] 1 2 1 [
    Battlecry $ \_ ->
        A $ Minion' [] $ \target ->
            Effect $ Enchant target $ Limited $ Until EndOfTurn $ statsDelta 2 0 ]


amaniBerserker :: MinionCard
amaniBerserker = mkMinion Common Neutral AmaniBerserker [] 2 2 3 [
    Enrage [] [
        statsDelta 3 0 ]]


al'AkirTheWindlord :: MinionCard
al'AkirTheWindlord = mkMinion Legendary Shaman Al'AkirTheWindlord [] 8 3 5 [
    Windfury,
    Charge,
    DivineShield,
    Taunt ]


aldorPeacekeeper :: MinionCard
aldorPeacekeeper = mkMinion Rare Paladin AldorPeacekeeper [] 3 3 3 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                A $ Minion' [OwnedBy opponent] $ \target ->
                    Effect $ Enchant target $ Continuous $ ChangeStat (Left 1) ]


ancientOfLore :: MinionCard
ancientOfLore = mkMinion Epic Druid AncientOfLore [] 7 5 5 [
    ChooseOne $ \this -> [
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1,
        A $ Character' [] $ \character ->
            Effect $ RestoreHealth character 5 ]]


ancientWatcher :: MinionCard
ancientWatcher = mkMinion Rare Neutral AncientWatcher [] 2 4 5 [
    Can'tAttack ]


arcaneGolem :: MinionCard
arcaneGolem = mkMinion Rare Neutral ArcaneGolem [] 3 4 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ GainManaCrystals opponent 1 CrystalFull ]


argentCommander :: MinionCard
argentCommander = mkMinion Rare Neutral ArgentCommander [] 6 4 2 [
    Charge,
    DivineShield ]


argentProtector :: MinionCard
argentProtector = mkMinion Common Paladin ArgentProtector [] 2 2 2 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion' [OwnedBy you] $ \target ->
                Effect $ Enchant target $ Continuous $ Grant DivineShield ]


argentSquire :: MinionCard
argentSquire = mkMinion Common Neutral ArgentSquire [] 1 1 1 [
    DivineShield ]


armorsmith :: MinionCard
armorsmith = mkMinion Rare Warrior Armorsmith [] 2 1 4 [
    WheneverMinion $ \this ->
        DamageIsDealt $ \victim _ _ ->
            OwnerOf this $ \you ->
                Effect $ when (victim `Satisfies` [OwnedBy you, IsMinion]) $ GainArmor you 1 ]


ashbringer :: WeaponCard
ashbringer = uncollectible $ mkWeapon Legendary Paladin Ashbringer 5 5 3 []


azureDrake :: MinionCard
azureDrake = mkMinion Rare Neutral AzureDrake [Dragon] 5 4 4 [
    SpellDamage 1,
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


baronGeddon :: MinionCard
baronGeddon = mkMinion Legendary Neutral BaronGeddon [] 7 7 5 [
    WheneverMinion $ \this ->
        EndOfTurnEvent $ \player ->
            OwnerOf this $ \you ->
                Effect $ when (player `Satisfies` [Is you]) $ Elect $ All $ Characters [Not $ MinionCharacter this] $ \characters ->
                    Effect $ ForEachCharacter characters $ \character ->
                        (this `damages` character) 2 ]


battleRage :: SpellCard
battleRage = mkSpell Common Warrior BattleRage 2 $ \this ->
    OwnerOf this $ \you ->
        All $ Characters [Damaged, OwnedBy you] $ \friendlies ->
            Effect $ ForEachCharacter friendlies $ \_ ->
                DrawCards you 1


bigGameHunter :: MinionCard
bigGameHunter = mkMinion Epic Neutral BigGameHunter [] 5 4 2 [
    Battlecry $ \_ ->
        A $ Minion' [RequireMinion (WithAttack GreaterEqual 7)] $ \target ->
            Effect $ DestroyMinion target ]


bite :: SpellCard
bite = mkSpell Rare Druid Bite 4 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Sequence [
            Enchant you $ Limited $ Until EndOfTurn $ statsDelta 4 0,
            GainArmor you 4 ]


blessedChampion :: SpellCard
blessedChampion = mkSpell Rare Paladin BlessedChampion 5 $ \_ ->
    A $ Minion' [] $ \target ->
        Effect $ Enchant target $ Continuous $ StatsScale 2 1


blizzard :: SpellCard
blizzard = mkSpell Rare Mage Blizzard 6 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ ForEachMinion victims $ \victim -> Sequence [
                    (this `damages` victim) 2,
                    freeze victim ]


bloodmageThalnos :: MinionCard
bloodmageThalnos = mkMinion Legendary Neutral BloodmageThalnos [] 2 1 1 [
    SpellDamage 1,
    Deathrattle $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


brawl :: SpellCard
brawl = mkSpell Epic Warrior Brawl 5 $ \_ ->
    Effect $ Elect $ A $ Minion' [] $ \survivor ->
        A $ Minion' [Not survivor] $ \someNonSurvivor ->
            All $ Minions [Not survivor] $ \victims ->
                Effect $ Sequence [
                    Unreferenced someNonSurvivor, -- This is because Brawl requires at least 2 minions to play.
                    ForEachMinion victims $ \victim ->
                        DestroyMinion victim ]


circleOfHealing :: SpellCard
circleOfHealing = mkSpell Common Priest CircleOfHealing 0 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ ForEachMinion minions $ \minion ->
            RestoreHealth (MinionCharacter minion) 4


coldlightOracle :: MinionCard
coldlightOracle = mkMinion Rare Neutral ColdlightOracle [Murloc] 3 2 2[
    Battlecry $ \_ ->
        All $ Players [] $ \players ->
            Effect $ ForEachPlayer players $ \player ->
                DrawCards player 2 ]


crazedAlchemist :: MinionCard
crazedAlchemist = mkMinion Rare Neutral CrazedAlchemist [] 2 2 2 [
    Battlecry $ \_ ->
        A $ Minion' [] $ \target ->
            Effect $ Enchant target $ Continuous SwapStats ]


cruelTaskmaster :: MinionCard
cruelTaskmaster = mkMinion Common Warrior CruelTaskmaster [] 2 2 2 [
    Battlecry $ \this ->
        A $ Minion' [] $ \target ->
            Effect $ Sequence [
                (this `damages` target) 1,
                Enchant target $ Continuous $ statsDelta 2 0 ]]


darkIronDwarf :: MinionCard
darkIronDwarf = mkMinion Common Neutral DarkIronDwarf [] 4 4 4 [
    Battlecry $ \_ ->
        A $ Minion' [] $ \target ->
            Effect $ Enchant target $ Limited $ Until EndOfTurn $ statsDelta 2 0 ]


direWolfAlpha :: MinionCard
direWolfAlpha = mkMinion Common Neutral DireWolfAlpha [Beast] 2 2 2 [
    AuraMinion $ \this ->
        EachMinion [AdjacentTo this] $ \minion ->
            Has minion $ statsDelta 1 0 ]


earthenRingFarseer :: MinionCard
earthenRingFarseer = mkMinion Common Neutral EarthenRingFarseer [] 3 3 3 [
    Battlecry $ \_ ->
        A $ Character' [] $ \character ->
            Effect $ RestoreHealth character 3 ]


earthShock :: SpellCard
earthShock = mkSpell Common Shaman EarthShock 1 $ \this ->
    A $ Minion' [] $ \target ->
        Effect $ Sequence [
            Silence target,
            (this `damages` target) 1 ]


emeraldDrake :: MinionCard
emeraldDrake = uncollectible $ mkMinion Free Hunter EmeraldDrake [Dragon] 4 7 6 []


equality :: SpellCard
equality = mkSpell Rare Paladin Equality 2 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ ForEachMinion minions $ \minion ->
            Enchant minion $ Continuous $ ChangeStat (Right 1)


fenCreeper :: MinionCard
fenCreeper = mkMinion Common Neutral FenCreeper [] 5 3 6 [
    Taunt ]


flameImp :: MinionCard
flameImp = mkMinion Common Warlock FlameImp [Demon] 1 3 2 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ (this `damages` you) 3 ]


frostElemental :: MinionCard
frostElemental = mkMinion Common Neutral FrostElemental [] 6 5 5 [
    Battlecry $ \_ ->
        A $ Character' [] $ \victim ->
            Effect $ Freeze victim ]


gadgetzanAuctioneer :: MinionCard
gadgetzanAuctioneer = mkMinion Rare Neutral GadgetzanAuctioneer [] 6 4 4 [
    WheneverMinion $ \this ->
        SpellIsCast $ \spell ->
            OwnerOf this $ \you ->
                Effect $ when (spell `Satisfies` [OwnedBy you]) $ DrawCards you 1 ]


gnoll :: MinionCard
gnoll = uncollectible $ mkMinion Free Neutral Gnoll [] 2 2 2 [
    Taunt ]


grommashHellscream :: MinionCard
grommashHellscream = mkMinion Legendary Warrior GrommashHellscream [] 8 4 9 [
    Charge,
    Enrage [] [
        statsDelta 6 0 ]]


gruul :: MinionCard
gruul = mkMinion Legendary Neutral Gruul [] 8 7 7 [
    WheneverMinion $ \this ->
        EndOfTurnEvent $ \_ ->
            Effect $ Enchant this $ Continuous $ statsDelta 1 1 ]


hogger :: MinionCard
hogger = mkMinion Legendary Neutral Hogger [] 6 4 4 [
    WheneverMinion $ \this ->
        EndOfTurnEvent $ \player ->
            OwnerOf this $ \you ->
                Effect $ when (player `Satisfies` [Is you]) $ (Summon gnoll) $ Rightmost you ]


holyFire :: SpellCard
holyFire = mkSpell Rare Priest HolyFire 6 $ \this ->
    A $ Character' [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 5,
                RestoreHealth (PlayerCharacter you) 5 ]


infernal :: MinionCard
infernal = uncollectible $ mkMinion Common Warlock Infernal [Demon] 6 6 6 []


injuredBlademaster :: MinionCard
injuredBlademaster = mkMinion Rare Neutral InjuredBlademaster [] 3 4 7 [
    Battlecry $ \this ->
        Effect $ (this `damages` this) 4 ]


innerRage :: SpellCard
innerRage = mkSpell Common Warrior InnerRage 0 $ \this ->
    A $ Minion' [] $ \target ->
        Effect $ Sequence [
            (this `damages` target) 1,
            Enchant target $ Continuous $ statsDelta 2 0 ]


ironbeakOwl :: MinionCard
ironbeakOwl = mkMinion Common Neutral IronbeakOwl [Beast] 3 2 1 [
    Battlecry $ \_ ->
        A $ Minion' [] $ \target ->
            Effect $ Silence target ]


layOnHands :: SpellCard
layOnHands = mkSpell Epic Paladin LayOnHands 8 $ \this ->
    A $ Character' [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                RestoreHealth target 6,
                DrawCards you 3 ]


leperGnome :: MinionCard
leperGnome = mkMinion Common Neutral LeperGnome [] 1 1 1 [
    Deathrattle $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ (this `damages` opponent) 2 ]


lootHoarder :: MinionCard
lootHoarder = mkMinion Common Neutral LootHoarder [] 2 2 1 [
    Deathrattle $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


malygos :: MinionCard
malygos = mkMinion Legendary Neutral Malygos [Dragon] 9 4 12 [
    SpellDamage 5 ]


markOfNature :: SpellCard
markOfNature = mkSpell Common Druid MarkOfNature 3 $ \_ ->
    ChooseOne' [
        A $ Minion' [] $ \target ->
            Effect $ Enchant target $ Continuous $ statsDelta 4 0,
        A $ Minion' [] $ \target ->
            Effect $ Sequence [
                Enchant target $ Continuous $ statsDelta 0 4,
                Enchant target $ Continuous $ Grant Taunt ]]


massDispel :: SpellCard
massDispel = mkSpell Rare Priest MassDispel 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ Sequence [
                    ForEachMinion victims $ \victim ->
                        Silence victim,
                    DrawCards you 1 ]


mogu'shanWarden :: MinionCard
mogu'shanWarden = mkMinion Common Neutral Mogu'shanWarden [] 4 1 7 [
    Taunt ]


naturalize :: SpellCard
naturalize = mkSpell Common Druid Naturalize 1 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            A $ Minion' [] $ \target ->
                Effect $ Sequence [
                    DestroyMinion target,
                    DrawCards opponent 2 ]


nourish :: SpellCard
nourish = mkSpell Rare Druid Nourish 5 $ \this ->
    OwnerOf this $ \you ->
        ChooseOne' [
            Effect $ GainManaCrystals you 2 CrystalFull,
            Effect $ DrawCards you 3 ]


pitLord :: MinionCard
pitLord = mkMinion Epic Warlock PitLord [Demon] 4 5 6 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ (this `damages` you) 5 ]


priestessOfElune :: MinionCard
priestessOfElune = mkMinion Common Neutral PriestessOfElune [] 6 5 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ RestoreHealth (PlayerCharacter you) 4 ]


pyroblast :: SpellCard
pyroblast = mkSpell Epic Mage Pyroblast 10 $ \this ->
    A $ Character' [] $ \target ->
        Effect $ (this `damages` target) 10


ragnarosTheFirelord :: MinionCard
ragnarosTheFirelord = mkMinion Legendary Neutral RagnarosTheFirelord [] 8 8 8 [
    Can'tAttack,
    WheneverMinion $ \this ->
        EndOfTurnEvent $ \player ->
            OwnerOf this $ \you ->
                OpponentOf you $ \opponent ->
                    Effect $ when (player `Satisfies` [Is you]) $ Elect $ A $ Character' [OwnedBy opponent] $ \enemy ->
                        Effect $ (this `damages` enemy) 8 ]


rampage :: SpellCard
rampage = mkSpell Common Warrior Rampage 2 $ \_ ->
    A $ Minion' [RequireMinion Damaged] $ \target ->
        Effect $ Enchant target $ Continuous $ statsDelta 3 3


scarletCrusader :: MinionCard
scarletCrusader = mkMinion Common Neutral ScarletCrusader [] 3 3 1 [
    DivineShield ]


shieldbearer :: MinionCard
shieldbearer = mkMinion Common Neutral Shieldbearer [] 1 0 4 [
    Taunt ]


silence :: SpellCard
silence = mkSpell Common Priest Classic.Silence 0 $ \_ ->
    A $ Minion' [] $ \target ->
        Effect $ Silence target


silvermoonGuardian :: MinionCard
silvermoonGuardian = mkMinion Common Neutral SilvermoonGuardian [] 4 3 3 [
    DivineShield ]


siphonSoul :: SpellCard
siphonSoul = mkSpell Rare Warlock SiphonSoul 6 $ \this ->
    A $ Minion' [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                DestroyMinion target,
                RestoreHealth (PlayerCharacter you) 3 ]


soulOfTheForest :: SpellCard
soulOfTheForest = mkSpell Common Druid SoulOfTheForest 4 $ \this ->
    OwnerOf this $ \you ->
        All $ Minions [OwnedBy you] $ \minions ->
            Effect $ ForEachMinion minions $ \minion ->
                Enchant minion $ Continuous $ Grant $ Deathrattle $ \this' ->
                    OwnerOf this' $ \owner ->
                        Effect $ (Summon treant_soulOfTheForest) $ Rightmost owner


spellbreaker :: MinionCard
spellbreaker = mkMinion Common Neutral Spellbreaker [] 4 4 3 [
    Battlecry $ \_ ->
        A $ Minion' [] $ \target ->
            Effect $ Silence target ]


stampedingKodo :: MinionCard
stampedingKodo = mkMinion Rare Neutral StampedingKodo [Beast] 5 3 5 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ Elect $ A $ Minion' [OwnedBy opponent, RequireMinion (WithAttack LessEqual 2)] $ \victim ->
                    Effect $ DestroyMinion victim ]


starfall :: SpellCard
starfall = mkSpell Rare Druid Starfall 5 $ \this ->
    ChooseOne' [
        A $ Minion' [] $ \target ->
            Effect $ (this `damages` target) 5,
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                All $ Minions [OwnedBy opponent] $ \victims ->
                    Effect $ ForEachMinion victims $ \victim ->
                        (this `damages` victim) 2 ]


sunwalker :: MinionCard
sunwalker = mkMinion Rare Neutral Sunwalker [] 6 4 5 [
    Taunt,
    DivineShield ]


taurenWarrior :: MinionCard
taurenWarrior = mkMinion Common Neutral TaurenWarrior [] 3 2 3 [
    Taunt,
    Enrage [] [
        statsDelta 3 0 ]]


templeEnforcer :: MinionCard
templeEnforcer = mkMinion Common Priest TempleEnforcer [] 6 6 6 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion' [OwnedBy you] $ \target ->
                Effect $ Enchant target $ Continuous $ statsDelta 0 3 ]


tirionFordring :: MinionCard
tirionFordring = mkMinion Legendary Paladin TirionFordring [] 8 6 6 [
    DivineShield,
    Taunt,
    Deathrattle $ \this ->
        OwnerOf this $ \you ->
            Effect $ EquipWeapon you ashbringer ]


treant_soulOfTheForest :: MinionCard
treant_soulOfTheForest = uncollectible $ mkMinion Free Druid Treant_SoulOfTheForest [] 1 2 2 []


twistingNether :: SpellCard
twistingNether = mkSpell Epic Warlock TwistingNether 8 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ ForEachMinion minions $ \minion ->
            DestroyMinion minion


windfuryHarpy :: MinionCard
windfuryHarpy = mkMinion Common Neutral WindfuryHarpy [] 6 4 5 [
    Windfury ]


wisp :: MinionCard
wisp = mkMinion Common Neutral Wisp [] 0 1 1 []


wrath :: SpellCard
wrath = mkSpell Common Druid Wrath 2 $ \this ->
    ChooseOne' [
        A $ Minion' [] $ \target ->
            Effect $ (this `damages` target) 3,
        A $ Minion' [] $ \target ->
            OwnerOf this $ \you ->
                Effect $ Sequence [
                    (this `damages` target) 1,
                    DrawCards you 1 ]]










