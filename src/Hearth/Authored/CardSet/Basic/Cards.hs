{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Hearth.Authored.CardSet.Basic.Cards (
    cards,
    healingTotem,
    searingTotem,
    silverHandRecruit,
    stoneclawTotem,
    theCoin,
    wickedKnife,
    wrathOfAirTotem,
) where


--------------------------------------------------------------------------------


import Hearth.Authored.CardSet.Basic.Names hiding (Charge, Windfury)
import Hearth.Combinator.Authoring
import Hearth.Model.Authoring
import Prelude hiding (sequence)

import qualified Hearth.Authored.CardSet.Basic.Names as Basic


--------------------------------------------------------------------------------


cards :: [Card]
cards = let x = toCard in [
    x acidicSwampOoze,
    x ancestralHealing,
    x animalCompanion,
    x arcaneExplosion,
    x arcaneIntellect,
    x arcaneMissiles,
    x arcaneShot,
    x arcaniteReaper,
    x archmage,
    x assassinate,
    x assassin'sBlade,
    x backstab,
    x blessingOfKings,
    x blessingOfMight,
    x bloodfenRaptor,
    x bloodlust,
    x bluegillWarrior,
    x boar,
    x bootyBayBodyguard,
    x boulderfistOgre,
    x charge,
    x chillwindYeti,
    x claw,
    x cleave,
    x consecration,
    x coreHound,
    x corruption,
    x dalaranMage,
    x darkscaleHealer,
    x deadlyPoison,
    x deadlyShot,
    x divineSpirit,
    x dragonlingMechanic,
    x drainLife,
    x dreadInfernal,
    x elvenArcher,
    x excessMana,
    x execute,
    x fanOfKnives,
    x fireball,
    x fireElemental,
    x flamestrike,
    x flametongueTotem,
    x frog,
    x frostbolt,
    x frostNova,
    x frostShock,
    x frostwolfGrunt,
    x frostwolfWarlord,
    x gnomishInventor,
    x goldshireFootman,
    x grimscaleOracle,
    x guardianOfKings,
    x gurubashiBerserker,
    x hammerOfWrath,
    x handOfProtection,
    x healingTotem,
    x healingTouch,
    x hellfire,
    x heroicStrike,
    x hex,
    x holyLight,
    x holyNova,
    x holySmite,
    x houndmaster,
    x huffer,
    x humility,
    x hunter'sMark,
    x ironbarkProtector,
    x innervate,
    x ironforgeRifleman,
    x killCommand,
    x koboldGeomancer,
    x kor'kronElite,
    x leokk,
    x light'sJustice,
    x lordOfTheArena,
    x magmaRager,
    x markOfTheWild,
    x mechanicalDragonling,
    x mindBlast,
    x mindControl,
    x mirrorImage_minion,
    x mirrorImage_spell,
    x misha,
    x moonfire,
    x mortalCoil,
    x multiShot,
    x murlocRaider,
    x murlocScout,
    x murlocTidehunter,
    x nightblade,
    x northshireCleric,
    x noviceEngineer,
    x oasisSnapjaw,
    x ogreMagi,
    x polymorph,
    x powerWordShield,
    x raidLeader,
    x razorfenHunter,
    x recklessRocketeer,
    x riverCrocolisk,
    x rockbiterWeapon,
    x sacrificialPact,
    x savageRoar,
    x searingTotem,
    x sen'jinShieldmasta,
    x shadowBolt,
    x shadowWordDeath,
    x shadowWordPain,
    x shatteredSunCleric,
    x sheep,
    x shieldBlock,
    x shiv,
    x silverbackPatriarch,
    x silverHandRecruit,
    x sinisterStrike,
    x soulfire,
    x sprint,
    x starfire,
    x stoneclawTotem,
    x stonetuskBoar,
    x stormpikeCommando,
    x stormwindChampion,
    x stormwindKnight,
    x succubus,
    x swipe,
    x theCoin,
    x timberWolf,
    x totemicMight,
    x tundraRhino,
    x voidwalker,
    x voodooDoctor,
    x warGolem,
    x warsongCommander,
    x waterElemental,
    x whirlwind,
    x wickedKnife,
    x wildGrowth,
    x windfury,
    x windspeaker,
    x wolfRider,
    x wrathOfAirTotem ]


--------------------------------------------------------------------------------


mkMinion :: Class -> BasicCardName -> [Tribe] -> Mana -> Attack -> Health -> [Ability 'Minion'] -> MinionCard
mkMinion = mkMinion' BasicCardName Free


mkSpell :: Class -> BasicCardName -> Mana -> SpellEffect -> SpellCard
mkSpell = mkSpell' BasicCardName Free


mkWeapon :: Class -> BasicCardName -> Mana -> Attack -> Durability -> [Ability 'Weapon'] -> WeaponCard
mkWeapon = mkWeapon' BasicCardName Free


--------------------------------------------------------------------------------


acidicSwampOoze :: MinionCard
acidicSwampOoze = mkMinion Neutral AcidicSwampOoze [] _2 _3 _2 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            opponentOf you $ \opponent ->
                A $ Weapon [OwnedBy opponent] $ \weapon ->
                    Effect $ destroy weapon ]


ancestralHealing :: SpellCard
ancestralHealing = mkSpell Shaman AncestralHealing _0 $ \_ ->
    A $ Minion [] $ \minion ->
        Effect $ sequence [
            RestoreToFullHealth $ asCharacter minion,
            enchant minion $ Grant Taunt ]


animalCompanion :: SpellCard
animalCompanion = mkSpell Hunter AnimalCompanion _3 $ \this ->
    ownerOf this $ \you ->
        Effect $ Get $ ChooseOne' $ map (\minion -> Effect $ (Summon minion) $ Rightmost you) [
            huffer,
            leokk,
            misha ]


arcaneExplosion :: SpellCard
arcaneExplosion = mkSpell Mage ArcaneExplosion _2 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \enemies ->
                Effect $ forEach enemies $ \enemy ->
                    (this `damages` enemy) _1


arcaneIntellect :: SpellCard
arcaneIntellect = mkSpell Mage ArcaneIntellect _3 $ \this ->
    ownerOf this $ \you ->
        Effect $ DrawCards you 2


arcaneMissiles :: SpellCard
arcaneMissiles = mkSpell Mage ArcaneMissiles _1 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            Effect $ RandomMissiles [OwnedBy opponent] 3 this


arcaneShot :: SpellCard
arcaneShot = mkSpell Hunter ArcaneShot _1 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) _2


arcaniteReaper :: WeaponCard
arcaniteReaper = mkWeapon Warrior ArcaniteReaper _5 _5 _2 []


archmage :: MinionCard
archmage = mkMinion Neutral Archmage [] _6 _4 _7 [
    SpellDamage 1 ]


assassinate :: SpellCard
assassinate = mkSpell Rogue Assassinate _5 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ destroy target


assassin'sBlade :: WeaponCard
assassin'sBlade = mkWeapon Rogue Assassin'sBlade _5 _3 _4 []


backstab :: SpellCard
backstab = mkSpell Rogue Backstab _0 $ \this ->
    A $ Minion [undamaged] $ \target ->
        Effect $ (this `damages` target) _2


blessingOfKings :: SpellCard
blessingOfKings = mkSpell Paladin BlessingOfKings _4 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ sequence [
            enchant target $ gainAttack _4,
            enchant target $ GainHealth _4 ]


blessingOfMight :: SpellCard
blessingOfMight = mkSpell Paladin BlessingOfMight _1 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ enchant target $ gainAttack _3


bloodfenRaptor :: MinionCard
bloodfenRaptor = mkMinion Neutral BloodfenRaptor [Beast] _2 _3 _2 []


bloodlust :: SpellCard
bloodlust = mkSpell Shaman Bloodlust _5 $ \this ->
    ownerOf this $ \you ->
        All $ Minions [OwnedBy you] $ \minions ->
            Effect $ forEach minions $ \minion ->
                enchant minion $ Until EndOfTurn $ gainAttack _3


bluegillWarrior :: MinionCard
bluegillWarrior = mkMinion Neutral BluegillWarrior [Murloc] _2 _2 _1 [
    Charge ]


boar :: MinionCard
boar = uncollectible $ mkMinion Neutral Boar [Beast] _1 _1 _1 []


bootyBayBodyguard :: MinionCard
bootyBayBodyguard = mkMinion Neutral BootyBayBodyguard [] _5 _5 _4 [
    Taunt ]


boulderfistOgre :: MinionCard
boulderfistOgre = mkMinion Neutral BoulderfistOgre [] _6 _6 _7 []


charge :: SpellCard
charge = mkSpell Warrior Basic.Charge _3 $ \this ->
    ownerOf this $ \you ->
        A $ Minion [OwnedBy you] $ \target ->
            Effect $ sequence [
                enchant target $ gainAttack _2,
                enchant target $ Grant Charge ]


chillwindYeti :: MinionCard
chillwindYeti = mkMinion Neutral ChillwindYeti [] _4 _4 _5 []


claw :: SpellCard
claw = mkSpell Druid Claw _1 $ \this ->
    ownerOf this $ \you ->
        Effect $ sequence [
            enchant you $ Until EndOfTurn $ gainAttack _2,
            GainArmor you _2 ]


cleave :: SpellCard
cleave = mkSpell Warrior Cleave _2 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            Effect $ Get $ A $ Minion [OwnedBy opponent] $ \victim1 ->
                A $ Minion [OwnedBy opponent, Not victim1] $ \victim2 ->
                    Effect $ forEach (handleList [victim1, victim2]) $ \victim ->
                        (this `damages` victim) _2


consecration :: SpellCard
consecration = mkSpell Paladin Consecration _4 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            All $ Characters [OwnedBy opponent] $ \enemies ->
                Effect $ forEach enemies $ \enemy ->
                    (this `damages` enemy) _2


coreHound :: MinionCard
coreHound = mkMinion Neutral CoreHound [Beast] _7 _9 _5 []


corruption :: SpellCard
corruption = mkSpell Warlock Corruption _1 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            A $ Minion [OwnedBy opponent] $ \target ->
                Effect $ enchant target $ DelayedEffect (Delay 1 BeginOfTurn) $ destroy target
    


dalaranMage :: MinionCard
dalaranMage = mkMinion Neutral DalaranMage [] _3 _1 _4 [
    SpellDamage 1 ]


darkscaleHealer :: MinionCard
darkscaleHealer = mkMinion Neutral DarkscaleHealer [] _5 _4 _5 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            All $ Characters [OwnedBy you] $ \friendlies ->
                Effect $ forEach friendlies $ \friendly ->
                    RestoreHealth friendly _2 ]


deadlyPoison :: SpellCard
deadlyPoison = mkSpell Rogue DeadlyPoison _1 $ \this ->
    ownerOf this $ \you ->
        A $ Weapon [OwnedBy you] $ \weapon ->
            Effect $ enchant weapon $ AttackDelta _2


deadlyShot :: SpellCard
deadlyShot = mkSpell Hunter DeadlyShot _3 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            Effect $ Get $ A $ Minion [OwnedBy opponent] $ \victim ->
                Effect $ destroy victim


divineSpirit :: SpellCard
divineSpirit = mkSpell Priest DivineSpirit _2 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ enchant target $ StatsScale _1 _2


dragonlingMechanic :: MinionCard
dragonlingMechanic = mkMinion Neutral DragonlingMechanic [] _4 _2 _4 [
    Battlecry $ \this ->
        Effect $ (Summon mechanicalDragonling) $ RightOf this ]


drainLife :: SpellCard
drainLife = mkSpell Warlock DrainLife _3 $ \this ->
    A $ Character [] $ \target ->
        ownerOf this $ \you ->
            Effect $ sequence [
                (this `damages` target) _2,
                RestoreHealth (asCharacter you) _2 ]


dreadInfernal :: MinionCard
dreadInfernal = mkMinion Warlock DreadInfernal [Demon] _6 _6 _6 [
    Battlecry $ \this ->
        All $ Characters [Not (asCharacter this)] $ \victims ->
            Effect $ forEach victims $ \victim ->
                (this `damages` victim) _1 ]


elvenArcher :: MinionCard
elvenArcher = mkMinion Neutral ElvenArcher [] _1 _1 _1 [
    Battlecry $ \this ->
        A $ Character [] $ \target ->
            Effect $ (this `damages` target) _1 ]


excessMana :: SpellCard
excessMana = uncollectible $ mkSpell Druid ExcessMana _0 $ \this ->
    ownerOf this $ \you ->
        Effect $ DrawCards you 1


execute :: SpellCard
execute = mkSpell Warrior Execute _1 $ \_ ->
    A $ Minion [damaged] $ \target ->
        Effect $ destroy target


fanOfKnives :: SpellCard
fanOfKnives = mkSpell Rogue FanOfKnives _4 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \enemies ->
                Effect $ sequence [
                    forEach enemies $ \enemy ->
                        (this `damages` enemy) _1,
                    DrawCards you 1 ]


fireball :: SpellCard
fireball = mkSpell Mage Fireball _4 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) _6


fireElemental :: MinionCard
fireElemental = mkMinion Shaman FireElemental [] _6 _6 _5 [
    Battlecry $ \this ->
        A $ Character [] $ \target ->
            Effect $ (this `damages` target) _3 ]


flamestrike :: SpellCard
flamestrike = mkSpell Mage Flamestrike _7 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ forEach victims $ \victim ->
                    (this `damages` victim) _4


flametongueTotem :: MinionCard
flametongueTotem = mkMinion Shaman FlametongueTotem [Totem] _2 _0 _3 [
    aura $ \this ->
        EachMinion [AdjacentTo this] $ \minion ->
            Has minion $ gainAttack _2 ]


frog :: MinionCard
frog = uncollectible $ mkMinion Neutral Frog [Beast] _0 _0 _1 [
    Taunt ]


frostbolt :: SpellCard
frostbolt = mkSpell Mage Frostbolt _2 $ \this ->
    A $ Character [] $ \target ->
        Effect $ sequence [
            (this `damages` target) _3,
            Freeze target ]


frostNova :: SpellCard
frostNova = mkSpell Mage FrostNova _3 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ forEach victims $ \victim ->
                    Freeze (asCharacter victim)


frostShock :: SpellCard
frostShock = mkSpell Shaman FrostShock _1 $ \this ->
    A $ Character [] $ \target ->
        Effect $ sequence [
            (this `damages` target) _1,
            Freeze target ]


frostwolfGrunt :: MinionCard
frostwolfGrunt = mkMinion Neutral FrostwolfGrunt [] _2 _2 _2 [
    Taunt ]


frostwolfWarlord :: MinionCard
frostwolfWarlord = mkMinion Neutral FrostwolfWarlord [] _5 _4 _4 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            All $ Minions [OwnedBy you, Not this] $ \minions ->
                Effect $ forEach minions $ \_ ->
                    sequence [
                        enchant this $ gainAttack _1,
                        enchant this $ GainHealth _1 ]]


gnomishInventor :: MinionCard
gnomishInventor = mkMinion Neutral GnomishInventor [] _4 _2 _4 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            Effect $ DrawCards you 1 ]


goldshireFootman :: MinionCard
goldshireFootman = mkMinion Neutral GoldshireFootman [] _1 _1 _2 [
    Taunt ]


grimscaleOracle :: MinionCard
grimscaleOracle = mkMinion Neutral GrimscaleOracle [Murloc] _1 _1 _1 [
    aura $ \this ->
        EachMinion [Not this, OfTribe Murloc] $ \minion ->
            Has minion $ gainAttack _1 ]


guardianOfKings :: MinionCard
guardianOfKings = mkMinion Paladin GuardianOfKings [] _7 _5 _6 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            Effect $ RestoreHealth (asCharacter you) _6 ]


gurubashiBerserker :: MinionCard
gurubashiBerserker = mkMinion Neutral GurubashiBerserker [] _5 _2 _7 [
    observer $ \this ->
        DamageIsDealt $ \victim _ _ ->
            Effect $ when (asCharacter this `Satisfies` [Is victim]) $ enchant this $ gainAttack _3 ]


hammerOfWrath :: SpellCard
hammerOfWrath = mkSpell Paladin HammerOfWrath _4 $ \this ->
    A $ Character [] $ \target ->
        ownerOf this $ \you ->
            Effect $ sequence [
                (this `damages` target) _3,
                DrawCards you 1 ]


handOfProtection :: SpellCard
handOfProtection = mkSpell Paladin HandOfProtection _1 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ enchant target $ Grant DivineShield


healingTotem :: MinionCard
healingTotem = uncollectible $ mkMinion Shaman HealingTotem [Totem] _1 _0 _2 [
    observer $ \this ->
        EndOfTurnEvent $ \player ->
            ownerOf this $ \you ->
                Effect $ when (player `Satisfies` [Is you]) $ Get $ All $ Minions [OwnedBy you] $ \minions ->
                    Effect $ forEach minions $ \minion ->
                        RestoreHealth (asCharacter minion) _1 ]


healingTouch :: SpellCard
healingTouch = mkSpell Druid HealingTouch _3 $ \_ ->
    A $ Character [] $ \target ->
        Effect $ RestoreHealth target _8


hellfire :: SpellCard
hellfire = mkSpell Warlock Hellfire _4 $ \this ->
    All $ Characters [] $ \victims ->
        Effect $ forEach victims $ \victim ->
            (this `damages` victim) _3


heroicStrike :: SpellCard
heroicStrike = mkSpell Warrior HeroicStrike _2 $ \this ->
    ownerOf this $ \you ->
        Effect $ enchant you $ Until EndOfTurn $ gainAttack _4


hex :: SpellCard
hex = mkSpell Shaman Hex _3 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Transform target frog


holyLight :: SpellCard
holyLight = mkSpell Paladin HolyLight _2 $ \_ ->
    A $ Character [] $ \target ->
        Effect $ RestoreHealth target _6


holyNova :: SpellCard
holyNova = mkSpell Priest HolyNova _5 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            All $ Characters [OwnedBy you] $ \friendlies ->
                All $ Characters [OwnedBy opponent] $ \enemies ->
                    Effect $ sequence [
                        forEach enemies $ \enemy ->
                            (this `damages` enemy) _2,
                        forEach friendlies $ \friendly ->
                            RestoreHealth friendly _2 ]


holySmite :: SpellCard
holySmite = mkSpell Priest HolySmite _1 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) _2


houndmaster :: MinionCard
houndmaster = mkMinion Hunter Houndmaster [] _4 _4 _3 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            A $ Minion [OwnedBy you, OfTribe Beast] $ \beast ->
                Effect $ sequence [
                    enchant beast $ gainAttack _2,
                    enchant beast $ GainHealth _2,
                    enchant beast $ Grant Taunt ]]


huffer :: MinionCard
huffer = uncollectible $ mkMinion Hunter Huffer [Beast] _3 _4 _2 [
    Charge ]


humility :: SpellCard
humility = mkSpell Paladin Humility _1 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ enchant target $ ChangeStat (Left _1)


hunter'sMark :: SpellCard
hunter'sMark = mkSpell Hunter Hunter'sMark _1 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ enchant target $ ChangeStat (Right _1)


koboldGeomancer :: MinionCard
koboldGeomancer = mkMinion Neutral KoboldGeomancer [] _2 _2 _2 [
    SpellDamage 1 ]


kor'kronElite :: MinionCard
kor'kronElite = mkMinion Warrior Kor'kronElite [] _4 _4 _3 [
    Charge ]


innervate :: SpellCard
innervate = mkSpell Druid Innervate _0 $ \this ->
    ownerOf this $ \you ->
        Effect $ GainManaCrystals you 2 CrystalTemporary


ironbarkProtector :: MinionCard
ironbarkProtector = mkMinion Druid IronbarkProtector [] _8 _8 _8 [
    Taunt ]


ironforgeRifleman :: MinionCard
ironforgeRifleman = mkMinion Neutral IronforgeRifleman [] _3 _2 _2 [
    Battlecry $ \this ->
        A $ Character [] $ \target ->
            Effect $ (this `damages` target) _1 ]


killCommand :: SpellCard
killCommand = mkSpell Hunter KillCommand _3 $ \this ->
    ownerOf this $ \you ->
        A $ Character [] $ \victim -> let
            deal = this `damages` victim
            in Effect $ If (you `Satisfies` [HasMinion [OfTribe Beast]])
                (deal _5)
                (deal _3)


leokk :: MinionCard
leokk = uncollectible $ mkMinion Hunter Leokk [Beast] _3 _2 _4 [
    aura $ \this ->
        ownerOf this $ \you ->
            EachMinion [Not this, OwnedBy you] $ \minion ->
                Has minion $ gainAttack _1 ]


light'sJustice :: WeaponCard
light'sJustice = mkWeapon Paladin Light'sJustice _1 _1 _4 []


lordOfTheArena :: MinionCard
lordOfTheArena = mkMinion Neutral LordOfTheArena [] _6 _6 _5 [
    Taunt ]


markOfTheWild :: SpellCard
markOfTheWild = mkSpell Druid MarkOfTheWild _2 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ sequence [
            enchant target $ Grant Taunt,
            enchant target $ gainAttack _2,
            enchant target $ GainHealth _2 ]


magmaRager :: MinionCard
magmaRager = mkMinion Neutral MagmaRager [] _3 _5 _1 []


mechanicalDragonling :: MinionCard
mechanicalDragonling = uncollectible $ mkMinion Neutral MechanicalDragonling [Mech] _1 _2 _1 []


mindBlast :: SpellCard
mindBlast = mkSpell Priest MindBlast _2 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            Effect $ (this `damages` opponent) _5


mindControl :: SpellCard
mindControl = mkSpell Priest MindControl _10 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            A $ Minion [OwnedBy opponent] $ \victim ->
                Effect $ TakeControl you victim


mirrorImage_minion :: MinionCard
mirrorImage_minion = uncollectible $ mkMinion Mage MirrorImage_Minion [] _1 _0 _2 [
    Taunt ]


mirrorImage_spell :: SpellCard
mirrorImage_spell = mkSpell Mage MirrorImage_Spell _1 $ \this ->
    ownerOf this $ \you ->
        Effect $ sequence $ replicate 2 $ (Summon mirrorImage_minion) $ Rightmost you


misha :: MinionCard
misha = uncollectible $ mkMinion Hunter Misha [Beast] _3 _4 _4 [
    Taunt ]


moonfire :: SpellCard
moonfire = mkSpell Druid Moonfire _0 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) _1


-- TODO:
-- MortalCoil hitting a KnifeJuggler juggled to death minion (triggered from say your VioletTeacher)
-- Need to match this behavior - https://www.youtube.com/watch?v=MYGSoWbaIAM
-- Comprehensive explanation - https://www.youtube.com/watch?v=H3d_qlm4Xws
mortalCoil :: SpellCard
mortalCoil = mkSpell Warlock MortalCoil _1 $ \this ->
    ownerOf this $ \you ->
        A $ Minion [] $ \target -> let
            effect = (this `damages` target) _1
            in Effect $ Observing effect $ DamageIsDealt $ \victim _ source -> let
                condition = this `Satisfies` [IsDamageSource source]
                    `And` victim `Satisfies` [withHealth LessEqual _0]
                in Effect $ when condition $ DrawCards you 1


multiShot :: SpellCard
multiShot = mkSpell Hunter MultiShot _4 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            Effect $ Get $ A $ Minion [OwnedBy opponent] $ \victim1 ->
                A $ Minion [OwnedBy opponent, Not victim1] $ \victim2 ->
                    Effect $ forEach (handleList [victim1, victim2]) $ \victim ->
                        (this `damages` victim) _3


murlocRaider :: MinionCard
murlocRaider = mkMinion Neutral MurlocRaider [Murloc] _1 _2 _1 []


murlocScout :: MinionCard
murlocScout = uncollectible $ mkMinion Neutral MurlocScout [Murloc] _0 _1 _1 []


murlocTidehunter :: MinionCard
murlocTidehunter = mkMinion Neutral MurlocTidehunter [Murloc] _2 _2 _1 [
    Battlecry $ \this ->
        Effect $ (Summon murlocScout) $ RightOf this ]


nightblade :: MinionCard
nightblade = mkMinion Neutral Nightblade [] _5 _4 _4 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            opponentOf you $ \opponent ->
                Effect $ (this `damages` opponent) _3 ]


northshireCleric :: MinionCard
northshireCleric = mkMinion Priest NorthshireCleric [] _1 _1 _3 [
    observer $ \this ->
        HealthIsRestored $ \recipient _ ->
            ownerOf this $ \you ->
                Effect $ when (recipient `Satisfies` [IsMinion]) $ DrawCards you 1 ]


noviceEngineer :: MinionCard
noviceEngineer = mkMinion Neutral NoviceEngineer [] _2 _1 _1 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            Effect $ DrawCards you 1 ]


oasisSnapjaw :: MinionCard
oasisSnapjaw = mkMinion Neutral OasisSnapjaw [Beast] _4 _2 _7 []


ogreMagi :: MinionCard
ogreMagi = mkMinion Neutral OgreMagi [] _4 _4 _4 [
    SpellDamage 1 ]


polymorph :: SpellCard
polymorph = mkSpell Mage Polymorph _4 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Transform target sheep


powerWordShield :: SpellCard
powerWordShield = mkSpell Priest PowerWordShield _1 $ \this ->
    A $ Minion [] $ \target ->
        ownerOf this $ \you ->
            Effect $ sequence [
                enchant target $ GainHealth _2,
                DrawCards you 1 ]


raidLeader :: MinionCard
raidLeader = mkMinion Neutral RaidLeader [] _3 _2 _2 [
    aura $ \this ->
        ownerOf this $ \you ->
            EachMinion [OwnedBy you, Not this] $ \minion ->
                Has minion $ gainAttack _1 ]


razorfenHunter :: MinionCard
razorfenHunter = mkMinion Neutral RazorfenHunter [] _3 _2 _3 [
    Battlecry $ \this ->
        Effect $ (Summon boar) $ RightOf this ]


recklessRocketeer :: MinionCard
recklessRocketeer = mkMinion Neutral RecklessRocketeer [] _6 _5 _2 [
    Charge ]


riverCrocolisk :: MinionCard
riverCrocolisk = mkMinion Neutral RiverCrocolisk [Beast] _2 _2 _3 []


rockbiterWeapon :: SpellCard
rockbiterWeapon = mkSpell Shaman RockbiterWeapon _1 $ \this ->
    ownerOf this $ \you ->
        A $ Character [OwnedBy you] $ \target ->
            Effect $ enchant target $ Until EndOfTurn $ gainAttack _3


sacrificialPact :: SpellCard
sacrificialPact = mkSpell Warlock SacrificialPact _0 $ \this ->
    ownerOf this $ \you ->
        A $ Minion [OfTribe Demon] $ \demon ->
            Effect $ sequence [
                destroy demon,
                RestoreHealth (asCharacter you) _5 ]


savageRoar :: SpellCard
savageRoar = mkSpell Druid SavageRoar _3 $ \this ->
    ownerOf this $ \you ->
        All $ Characters [OwnedBy you] $ \friendlies ->
            Effect $ forEach friendlies $ \friendly ->
                enchant friendly $ Until EndOfTurn $ gainAttack _2


searingTotem :: MinionCard
searingTotem = uncollectible $ mkMinion Shaman SearingTotem [Totem] _1 _1 _1 []


sen'jinShieldmasta :: MinionCard
sen'jinShieldmasta = mkMinion Neutral Sen'jinShieldmasta [] _4 _3 _5 [
    Taunt ]


shadowBolt :: SpellCard
shadowBolt = mkSpell Warlock ShadowBolt _3 $ \this ->
    A $ Minion [] $ \target ->
        Effect $ (this `damages` target) _4


shadowWordDeath :: SpellCard
shadowWordDeath = mkSpell Priest ShadowWordDeath _3 $ \_ ->
    A $ Minion [withAttack GreaterEqual _5] $ \target ->
        Effect $ destroy target


shadowWordPain :: SpellCard
shadowWordPain = mkSpell Priest ShadowWordPain _2 $ \_ ->
    A $ Minion [withAttack LessEqual _3] $ \target ->
        Effect $ destroy target


shatteredSunCleric :: MinionCard
shatteredSunCleric = mkMinion Neutral ShatteredSunCleric [] _3 _3 _2 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            A $ Minion [OwnedBy you] $ \target ->
                Effect $ sequence [
                    enchant target $ gainAttack _1,
                    enchant target $ GainHealth _1 ]]


sheep :: MinionCard
sheep = uncollectible $ mkMinion Neutral Sheep [Beast] _0 _1 _1 []


shieldBlock :: SpellCard
shieldBlock = mkSpell Warrior ShieldBlock _3 $ \this ->
    ownerOf this $ \you ->
        Effect $ sequence [
            GainArmor you _5,
            DrawCards you 1 ]


shiv :: SpellCard
shiv = mkSpell Rogue Shiv _2 $ \this ->
    A $ Character [] $ \target ->
        ownerOf this $ \you ->
            Effect $ sequence [
                (this `damages` target) _1,
                DrawCards you 1 ]


silverbackPatriarch :: MinionCard
silverbackPatriarch = mkMinion Neutral SilverbackPatriarch [Beast] _3 _1 _4 [
    Taunt ]


silverHandRecruit :: MinionCard
silverHandRecruit = uncollectible $ mkMinion Paladin SilverHandRecruit [] _1 _1 _1 []


sinisterStrike :: SpellCard
sinisterStrike = mkSpell Rogue SinisterStrike _1 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            Effect $ (this `damages` opponent) _3


soulfire :: SpellCard
soulfire = mkSpell Warlock Soulfire _1 $ \this ->
    ownerOf this $ \you ->
        A $ Character [] $ \victim ->
            Effect $ sequence [
                (this `damages` victim) _4,
                DiscardAtRandom you ]


sprint :: SpellCard
sprint = mkSpell Rogue Sprint _7 $ \this ->
    ownerOf this $ \you ->
        Effect $ DrawCards you 4


starfire :: SpellCard
starfire = mkSpell Druid Starfire _6 $ \this ->
    A $ Character [] $ \target ->
        ownerOf this $ \you ->
            Effect $ sequence [
                (this `damages` target) _5,
                DrawCards you 1 ]


stoneclawTotem :: MinionCard
stoneclawTotem = uncollectible $ mkMinion Shaman StoneclawTotem [Totem] _1 _0 _2 [
    Taunt ]


stonetuskBoar :: MinionCard
stonetuskBoar = mkMinion Neutral StonetuskBoar [Beast] _1 _1 _1 [
    Charge ]


stormpikeCommando :: MinionCard
stormpikeCommando = mkMinion Neutral StormpikeCommando [] _5 _4 _2 [
    Battlecry $ \this ->
        A $ Character [] $ \target ->
            Effect $ (this `damages` target) _2 ]


stormwindKnight :: MinionCard
stormwindKnight = mkMinion Neutral StormwindKnight [] _4 _2 _5 [
    Charge ]


stormwindChampion :: MinionCard
stormwindChampion = mkMinion Neutral StormwindChampion [] _7 _6 _6 [
    aura $ \this ->
        ownerOf this $ \you ->
            EachMinion [OwnedBy you, Not this] $ \minion ->
                sequence [
                    Has minion $ gainAttack _1,
                    Has minion $ GainHealth _1 ]]


succubus :: MinionCard
succubus = mkMinion Warlock Succubus [Demon] _2 _4 _3 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            Effect $ DiscardAtRandom you ]


swipe :: SpellCard
swipe = mkSpell Druid Swipe _4 $ \this ->
    ownerOf this $ \you ->
        opponentOf you $ \opponent ->
            A $ Character [OwnedBy opponent] $ \target ->
                All $ Characters [OwnedBy opponent, Not target] $ \others ->
                    Effect $ sequence [
                        (this `damages` target) _4,
                        forEach others $ \other ->
                            (this `damages` other) _1 ]


theCoin :: SpellCard
theCoin = uncollectible $ mkSpell Neutral TheCoin _0 $ \this ->
    ownerOf this $ \you ->
        Effect $ GainManaCrystals you 1 CrystalTemporary


timberWolf :: MinionCard
timberWolf = mkMinion Hunter TimberWolf [Beast] _1 _1 _1 [
    aura $ \this ->
        ownerOf this $ \you ->
            EachMinion [OwnedBy you, Not this, OfTribe Beast] $ \minion ->
                Has minion $ gainAttack _1 ]


totemicMight :: SpellCard
totemicMight = mkSpell Shaman TotemicMight _0 $ \this ->
    ownerOf this $ \you ->
        All $ Minions [OwnedBy you, OfTribe Totem] $ \totems ->
            Effect $ forEach totems $ \totem ->
                enchant totem $ GainHealth _2


tundraRhino :: MinionCard
tundraRhino = mkMinion Hunter TundraRhino [Beast] _5 _2 _5 [
    aura $ \this ->
        ownerOf this $ \you ->
            EachMinion [OwnedBy you, OfTribe Beast] $ \minion ->
                HasAbility minion Charge ]


voidwalker :: MinionCard
voidwalker = mkMinion Warlock Voidwalker [Demon] _1 _1 _3 [
    Taunt ]


voodooDoctor :: MinionCard
voodooDoctor = mkMinion Neutral VoodooDoctor [] _1 _2 _1 [
    Battlecry $ \_ ->
        A $ Character [] $ \character ->
            Effect $ RestoreHealth character _2 ]


warGolem :: MinionCard
warGolem = mkMinion Neutral WarGolem [] _7 _7 _7 []


warsongCommander :: MinionCard
warsongCommander = mkMinion Warrior WarsongCommander [] _3 _2 _3 [
    aura $ \this ->
        ownerOf this $ \you ->
            EachMinion [OwnedBy you, HasCharge] $ \minion ->
                Has minion $ gainAttack _1 ]


waterElemental :: MinionCard
waterElemental = mkMinion Mage WaterElemental [] _4 _3 _6 [
    observer $ \this ->
        DamageIsDealt $ \victim _ source ->
            Effect $ when (this `Satisfies` [IsDamageSource source]) $ Freeze victim ]


whirlwind :: SpellCard
whirlwind = mkSpell Warrior Whirlwind _1 $ \this ->
    All $ Minions [] $ \minions ->
        Effect $ forEach minions $ \minion ->
            (this `damages` minion) _1


wickedKnife :: WeaponCard
wickedKnife = uncollectible $ mkWeapon Rogue WickedKnife _1 _1 _2 []


wildGrowth :: SpellCard
wildGrowth = mkSpell Druid WildGrowth _2 $ \this ->
    ownerOf this $ \you ->
        Effect $ If (you `Satisfies` [HasMaxManaCrystals])
            (PutInHand you $ CardSpell excessMana)
            $ GainManaCrystals you 1 CrystalEmpty


windfury :: SpellCard
windfury = mkSpell Shaman Basic.Windfury _2 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ enchant target $ Grant Windfury


windspeaker :: MinionCard
windspeaker = mkMinion Shaman Windspeaker [] _4 _3 _3 [
    Battlecry $ \this ->
        ownerOf this $ \you ->
            A $ Minion [OwnedBy you] $ \target ->
                Effect $ enchant target $ Grant Windfury ]


wolfRider :: MinionCard
wolfRider = mkMinion Neutral WolfRider [] _3 _3 _1 [
    Charge ]


wrathOfAirTotem :: MinionCard
wrathOfAirTotem = uncollectible $ mkMinion Shaman WrathOfAirTotem [Totem] _1 _0 _2 [
    SpellDamage 1 ]








