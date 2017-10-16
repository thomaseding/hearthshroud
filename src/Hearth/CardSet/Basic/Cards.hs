{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Hearth.CardSet.Basic.Cards (
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


import Hearth.Authoring.Combinators
import Hearth.CardName
import Hearth.CardSet.Basic.Names hiding (Charge, Windfury)
import qualified Hearth.CardSet.Basic.Names as Basic
import Hearth.Model


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


mkMinion :: Class -> BasicCardName -> [MinionType] -> Mana -> Attack -> Health -> [Ability Minion] -> MinionCard
mkMinion = mkMinion' BasicCardName Free


mkSpell :: Class -> BasicCardName -> Mana -> SpellEffect -> SpellCard
mkSpell = mkSpell' BasicCardName Free


mkWeapon :: Class -> BasicCardName -> Mana -> Attack -> Durability -> [Ability Weapon] -> WeaponCard
mkWeapon = mkWeapon' BasicCardName Free


--------------------------------------------------------------------------------


acidicSwampOoze :: MinionCard
acidicSwampOoze = mkMinion Neutral AcidicSwampOoze [] 2 3 2 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                A $ Weapon' [OwnedBy opponent] $ \weapon ->
                    Effect $ DestroyWeapon weapon ]


ancestralHealing :: SpellCard
ancestralHealing = mkSpell Shaman AncestralHealing 0 $ \_ ->
    A $ Minion' [] $ \minion ->
        Effect $ Sequence [
            RestoreToFullHealth $ MinionCharacter minion,
            Enchant minion $ Continuous $ Grant Taunt ]


animalCompanion :: SpellCard
animalCompanion = mkSpell Hunter AnimalCompanion 3 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Elect $ ChooseOne' $ map (\minion -> Effect $ (Summon minion) $ Rightmost you) [
            huffer,
            leokk,
            misha ]


arcaneExplosion :: SpellCard
arcaneExplosion = mkSpell Mage ArcaneExplosion 2 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \enemies ->
                Effect $ ForEachMinion enemies $ \enemy ->
                    (this `damages` enemy) 1


arcaneIntellect :: SpellCard
arcaneIntellect = mkSpell Mage ArcaneIntellect 3 $ \this ->
    OwnerOf this $ \you ->
        Effect $ DrawCards you 2


arcaneMissiles :: SpellCard
arcaneMissiles = mkSpell Mage ArcaneMissiles 1 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ RandomMissiles [OwnedBy opponent] 3 this


arcaneShot :: SpellCard
arcaneShot = mkSpell Hunter ArcaneShot 1 $ \this ->
    A $ Character' [] $ \target ->
        Effect $ (this `damages` target) 2


arcaniteReaper :: WeaponCard
arcaniteReaper = mkWeapon Warrior ArcaniteReaper 5 5 2 []


archmage :: MinionCard
archmage = mkMinion Neutral Archmage [] 6 4 7 [
    SpellDamage 1 ]


assassinate :: SpellCard
assassinate = mkSpell Rogue Assassinate 5 $ \_ ->
    A $ Minion' [] $ \target ->
        Effect $ DestroyMinion target


assassin'sBlade :: WeaponCard
assassin'sBlade = mkWeapon Rogue Assassin'sBlade 5 3 4 []


backstab :: SpellCard
backstab = mkSpell Rogue Backstab 0 $ \this ->
    A $ Minion' [RequireMinion Undamaged] $ \target ->
        Effect $ (this `damages` target) 2


blessingOfKings :: SpellCard
blessingOfKings = mkSpell Paladin BlessingOfKings 4 $ \_ ->
    A $ Minion' [] $ \target ->
        Effect $ Enchant target $ Continuous $ statsDelta 4 4


blessingOfMight :: SpellCard
blessingOfMight = mkSpell Paladin BlessingOfMight 1 $ \_ ->
    A $ Minion' [] $ \target ->
        Effect $ Enchant target $ Continuous $ statsDelta 3 0


bloodfenRaptor :: MinionCard
bloodfenRaptor = mkMinion Neutral BloodfenRaptor [Beast] 2 3 2 []


bloodlust :: SpellCard
bloodlust = mkSpell Shaman Bloodlust 5 $ \this ->
    OwnerOf this $ \you ->
        All $ Minions [OwnedBy you] $ \minions ->
            Effect $ ForEachMinion minions $ \minion ->
                Enchant minion $ Limited $ Until EndOfTurn $ statsDelta 3 0


bluegillWarrior :: MinionCard
bluegillWarrior = mkMinion Neutral BluegillWarrior [Murloc] 2 2 1 [
    Charge ]


boar :: MinionCard
boar = uncollectible $ mkMinion Neutral Boar [Beast] 1 1 1 []


bootyBayBodyguard :: MinionCard
bootyBayBodyguard = mkMinion Neutral BootyBayBodyguard [] 5 5 4 [
    Taunt ]


boulderfistOgre :: MinionCard
boulderfistOgre = mkMinion Neutral BoulderfistOgre [] 6 6 7 []


charge :: SpellCard
charge = mkSpell Warrior Basic.Charge 3 $ \this ->
    OwnerOf this $ \you ->
        A $ Minion' [OwnedBy you] $ \target ->
            Effect $ Sequence [
                Enchant target $ Continuous $ statsDelta 2 0,
                Enchant target $ Continuous $ Grant Charge ]


chillwindYeti :: MinionCard
chillwindYeti = mkMinion Neutral ChillwindYeti [] 4 4 5 []


claw :: SpellCard
claw = mkSpell Druid Claw 1 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Sequence [
            Enchant you $ Limited $ Until EndOfTurn $ statsDelta 2 0,
            GainArmor you 2 ]


cleave :: SpellCard
cleave = mkSpell Warrior Cleave 2 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ Elect $ A $ Minion' [OwnedBy opponent] $ \victim1 ->
                A $ Minion' [OwnedBy opponent, Not victim1] $ \victim2 ->
                    Effect $ ForEachMinion (handleList [victim1, victim2]) $ \victim ->
                        (this `damages` victim) 2


consecration :: SpellCard
consecration = mkSpell Paladin Consecration 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Characters [OwnedBy opponent] $ \enemies ->
                Effect $ ForEachCharacter enemies $ \enemy ->
                    (this `damages` enemy) 2


coreHound :: MinionCard
coreHound = mkMinion Neutral CoreHound [Beast] 7 9 5 []


corruption :: SpellCard
corruption = mkSpell Warlock Corruption 1 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            A $ Minion' [OwnedBy opponent] $ \target ->
                Effect $ Enchant target $ Limited $ DelayedEffect (Delay 1 BeginOfTurn) $ DestroyMinion target
    


dalaranMage :: MinionCard
dalaranMage = mkMinion Neutral DalaranMage [] 3 1 4 [
    SpellDamage 1 ]


darkscaleHealer :: MinionCard
darkscaleHealer = mkMinion Neutral DarkscaleHealer [] 5 4 5 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            All $ Characters [OwnedBy you] $ \friendlies ->
                Effect $ ForEachCharacter friendlies $ \friendly ->
                    RestoreHealth friendly 2 ]


deadlyPoison :: SpellCard
deadlyPoison = mkSpell Rogue DeadlyPoison 1 $ \this ->
    OwnerOf this $ \you ->
        A $ Weapon' [OwnedBy you] $ \weapon ->
            Effect $ Enchant weapon $ Continuous $ AttackDelta 2


deadlyShot :: SpellCard
deadlyShot = mkSpell Hunter DeadlyShot 3 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ Elect $ A $ Minion' [OwnedBy opponent] $ \victim ->
                Effect $ DestroyMinion victim


divineSpirit :: SpellCard
divineSpirit = mkSpell Priest DivineSpirit 2 $ \_ ->
    A $ Minion' [] $ \target ->
        Effect $ Enchant target $ Continuous $ StatsScale 1 2


dragonlingMechanic :: MinionCard
dragonlingMechanic = mkMinion Neutral DragonlingMechanic [] 4 2 4 [
    Battlecry $ \this ->
        Effect $ (Summon mechanicalDragonling) $ RightOf this ]


drainLife :: SpellCard
drainLife = mkSpell Warlock DrainLife 3 $ \this ->
    A $ Character' [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 2,
                RestoreHealth (PlayerCharacter you) 2 ]


dreadInfernal :: MinionCard
dreadInfernal = mkMinion Warlock DreadInfernal [Demon] 6 6 6 [
    Battlecry $ \this ->
        All $ Characters [Not (MinionCharacter this)] $ \victims ->
            Effect $ ForEachCharacter victims $ \victim ->
                (this `damages` victim) 1 ]


elvenArcher :: MinionCard
elvenArcher = mkMinion Neutral ElvenArcher [] 1 1 1 [
    Battlecry $ \this ->
        A $ Character' [] $ \target ->
            Effect $ (this `damages` target) 1 ]


excessMana :: SpellCard
excessMana = uncollectible $ mkSpell Druid ExcessMana 0 $ \this ->
    OwnerOf this $ \you ->
        Effect $ DrawCards you 1


execute :: SpellCard
execute = mkSpell Warrior Execute 1 $ \_ ->
    A $ Minion' [RequireMinion Damaged] $ \target ->
        Effect $ DestroyMinion target


fanOfKnives :: SpellCard
fanOfKnives = mkSpell Rogue FanOfKnives 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \enemies ->
                Effect $ Sequence [
                    ForEachMinion enemies $ \enemy ->
                        (this `damages` enemy) 1,
                    DrawCards you 1 ]


fireball :: SpellCard
fireball = mkSpell Mage Fireball 4 $ \this ->
    A $ Character' [] $ \target ->
        Effect $ (this `damages` target) 6


fireElemental :: MinionCard
fireElemental = mkMinion Shaman FireElemental [] 6 6 5 [
    Battlecry $ \this ->
        A $ Character' [] $ \target ->
            Effect $ (this `damages` target) 3 ]


flamestrike :: SpellCard
flamestrike = mkSpell Mage Flamestrike 7 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ ForEachMinion victims $ \victim ->
                    (this `damages` victim) 4


flametongueTotem :: MinionCard
flametongueTotem = mkMinion Shaman FlametongueTotem [Totem] 2 0 3 [
    AuraMinion $ \this ->
        EachMinion [AdjacentTo this] $ \minion ->
            Has minion $ statsDelta 2 0 ]


frog :: MinionCard
frog = uncollectible $ mkMinion Neutral Frog [Beast] 0 0 1 [
    Taunt ]


frostbolt :: SpellCard
frostbolt = mkSpell Mage Frostbolt 2 $ \this ->
    A $ Character' [] $ \target ->
        Effect $ Sequence [
            (this `damages` target) 3,
            Freeze target ]


frostNova :: SpellCard
frostNova = mkSpell Mage FrostNova 3 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ ForEachMinion victims $ \victim ->
                    Freeze (MinionCharacter victim)


frostShock :: SpellCard
frostShock = mkSpell Shaman FrostShock 1 $ \this ->
    A $ Character' [] $ \target ->
        Effect $ Sequence [
            (this `damages` target) 1,
            Freeze target ]


frostwolfGrunt :: MinionCard
frostwolfGrunt = mkMinion Neutral FrostwolfGrunt [] 2 2 2 [
    Taunt ]


frostwolfWarlord :: MinionCard
frostwolfWarlord = mkMinion Neutral FrostwolfWarlord [] 5 4 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            All $ Minions [OwnedBy you, Not this] $ \minions ->
                Effect $ ForEachMinion minions $ \_ ->
                    Enchant this $ Continuous $ statsDelta 1 1 ]


gnomishInventor :: MinionCard
gnomishInventor = mkMinion Neutral GnomishInventor [] 4 2 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


goldshireFootman :: MinionCard
goldshireFootman = mkMinion Neutral GoldshireFootman [] 1 1 2 [
    Taunt ]


grimscaleOracle :: MinionCard
grimscaleOracle = mkMinion Neutral GrimscaleOracle [Murloc] 1 1 1 [
    AuraMinion $ \this ->
        EachMinion [Not this, HasType Murloc] $ \minion ->
            Has minion $ statsDelta 1 0 ]


guardianOfKings :: MinionCard
guardianOfKings = mkMinion Paladin GuardianOfKings [] 7 5 6 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ RestoreHealth (PlayerCharacter you) 6 ]


gurubashiBerserker :: MinionCard
gurubashiBerserker = mkMinion Neutral GurubashiBerserker [] 5 2 7 [
    WheneverMinion $ \this ->
        DamageIsDealt $ \victim _ _ ->
            Effect $ when (MinionCharacter this `Satisfies` [Is victim]) $ Enchant this $ Continuous $ statsDelta 3 0 ]


hammerOfWrath :: SpellCard
hammerOfWrath = mkSpell Paladin HammerOfWrath 4 $ \this ->
    A $ Character' [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 3,
                DrawCards you 1 ]


handOfProtection :: SpellCard
handOfProtection = mkSpell Paladin HandOfProtection 1 $ \_ ->
    A $ Minion' [] $ \target ->
        Effect $ Enchant target $ Continuous $ Grant DivineShield


healingTotem :: MinionCard
healingTotem = uncollectible $ mkMinion Shaman HealingTotem [Totem] 1 0 2 [
    WheneverMinion $ \this ->
        EndOfTurnEvent $ \player ->
            OwnerOf this $ \you ->
                Effect $ when (player `Satisfies` [Is you]) $ Elect $ All $ Minions [OwnedBy you] $ \minions ->
                    Effect $ ForEachMinion minions $ \minion ->
                        RestoreHealth (MinionCharacter minion) 1 ]


healingTouch :: SpellCard
healingTouch = mkSpell Druid HealingTouch 3 $ \_ ->
    A $ Character' [] $ \target ->
        Effect $ RestoreHealth target 8


hellfire :: SpellCard
hellfire = mkSpell Warlock Hellfire 4 $ \this ->
    All $ Characters [] $ \victims ->
        Effect $ ForEachCharacter victims $ \victim ->
            (this `damages` victim) 3


heroicStrike :: SpellCard
heroicStrike = mkSpell Warrior HeroicStrike 2 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Enchant you $ Limited $ Until EndOfTurn $ statsDelta 4 0


hex :: SpellCard
hex = mkSpell Shaman Hex 3 $ \_ ->
    A $ Minion' [] $ \target ->
        Effect $ Transform target frog


holyLight :: SpellCard
holyLight = mkSpell Paladin HolyLight 2 $ \_ ->
    A $ Character' [] $ \target ->
        Effect $ RestoreHealth target 6


holyNova :: SpellCard
holyNova = mkSpell Priest HolyNova 5 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Characters [OwnedBy you] $ \friendlies ->
                All $ Characters [OwnedBy opponent] $ \enemies ->
                    Effect $ Sequence [
                        ForEachCharacter enemies $ \enemy ->
                            (this `damages` enemy) 2,
                        ForEachCharacter friendlies $ \friendly ->
                            RestoreHealth friendly 2 ]


holySmite :: SpellCard
holySmite = mkSpell Priest HolySmite 1 $ \this ->
    A $ Character' [] $ \target ->
        Effect $ (this `damages` target) 2


houndmaster :: MinionCard
houndmaster = mkMinion Hunter Houndmaster [] 4 4 3 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion' [OwnedBy you, HasType Beast] $ \beast ->
                Effect $ Sequence [
                    Enchant beast $ Continuous $ statsDelta 2 2,
                    Enchant beast $ Continuous $ Grant Taunt ]]


huffer :: MinionCard
huffer = uncollectible $ mkMinion Hunter Huffer [Beast] 3 4 2 [
    Charge ]


humility :: SpellCard
humility = mkSpell Paladin Humility 1 $ \_ ->
    A $ Minion' [] $ \target ->
        Effect $ Enchant target $ Continuous $ ChangeStat (Left 1)


hunter'sMark :: SpellCard
hunter'sMark = mkSpell Hunter Hunter'sMark 1 $ \_ ->
    A $ Minion' [] $ \target ->
        Effect $ Enchant target $ Continuous $ ChangeStat (Right 1)


koboldGeomancer :: MinionCard
koboldGeomancer = mkMinion Neutral KoboldGeomancer [] 2 2 2 [
    SpellDamage 1 ]


kor'kronElite :: MinionCard
kor'kronElite = mkMinion Warrior Kor'kronElite [] 4 4 3 [
    Charge ]


innervate :: SpellCard
innervate = mkSpell Druid Innervate 0 $ \this ->
    OwnerOf this $ \you ->
        Effect $ GainManaCrystals you 2 CrystalTemporary


ironbarkProtector :: MinionCard
ironbarkProtector = mkMinion Druid IronbarkProtector [] 8 8 8 [
    Taunt ]


ironforgeRifleman :: MinionCard
ironforgeRifleman = mkMinion Neutral IronforgeRifleman [] 3 2 2 [
    Battlecry $ \this ->
        A $ Character' [] $ \target ->
            Effect $ (this `damages` target) 1 ]


killCommand :: SpellCard
killCommand = mkSpell Hunter KillCommand 3 $ \this ->
    OwnerOf this $ \you ->
        A $ Character' [] $ \victim -> let
            deal = this `damages` victim
            in Effect $ If (you `Satisfies` [HasMinion [HasType Beast]])
                (deal 5)
                (deal 3)


leokk :: MinionCard
leokk = uncollectible $ mkMinion Hunter Leokk [Beast] 3 2 4 [
    AuraMinion $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [Not this, OwnedBy you] $ \minion ->
                Has minion $ statsDelta 1 0 ]


light'sJustice :: WeaponCard
light'sJustice = mkWeapon Paladin Light'sJustice 1 1 4 []


lordOfTheArena :: MinionCard
lordOfTheArena = mkMinion Neutral LordOfTheArena [] 6 6 5 [
    Taunt ]


markOfTheWild :: SpellCard
markOfTheWild = mkSpell Druid MarkOfTheWild 2 $ \_ ->
    A $ Minion' [] $ \target ->
        Effect $ Sequence [
            Enchant target $ Continuous $ Grant Taunt,
            Enchant target $ Continuous $ statsDelta 2 2 ]


magmaRager :: MinionCard
magmaRager = mkMinion Neutral MagmaRager [] 3 5 1 []


mechanicalDragonling :: MinionCard
mechanicalDragonling = uncollectible $ mkMinion Neutral MechanicalDragonling [Mech] 1 2 1 []


mindBlast :: SpellCard
mindBlast = mkSpell Priest MindBlast 2 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ (this `damages` opponent) 5


mindControl :: SpellCard
mindControl = mkSpell Priest MindControl 10 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            A $ Minion' [OwnedBy opponent] $ \victim ->
                Effect $ TakeControl you victim


mirrorImage_minion :: MinionCard
mirrorImage_minion = uncollectible $ mkMinion Mage MirrorImage_Minion [] 1 0 2 [
    Taunt ]


mirrorImage_spell :: SpellCard
mirrorImage_spell = mkSpell Mage MirrorImage_Spell 1 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Sequence $ replicate 2 $ (Summon mirrorImage_minion) $ Rightmost you


misha :: MinionCard
misha = uncollectible $ mkMinion Hunter Misha [Beast] 3 4 4 [
    Taunt ]


moonfire :: SpellCard
moonfire = mkSpell Druid Moonfire 0 $ \this ->
    A $ Character' [] $ \target ->
        Effect $ (this `damages` target) 1


-- TODO:
-- MortalCoil hitting a KnifeJuggler juggled to death minion (triggered from say your VioletTeacher)
-- Need to match this behavior - https://www.youtube.com/watch?v=MYGSoWbaIAM
-- Comprehensive explanation - https://www.youtube.com/watch?v=H3d_qlm4Xws
mortalCoil :: SpellCard
mortalCoil = mkSpell Warlock MortalCoil 1 $ \this ->
    OwnerOf this $ \you ->
        A $ Minion' [] $ \target -> let
            effect = (this `damages` target) 1
            in Effect $ Observing effect $ DamageIsDealt $ \victim _ source -> let
                condition = this `Satisfies` [IsDamageSource source]
                    `And` victim `Satisfies` [WithHealth LessEqual 0]
                in Effect $ when condition $ DrawCards you 1


multiShot :: SpellCard
multiShot = mkSpell Hunter MultiShot 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ Elect $ A $ Minion' [OwnedBy opponent] $ \victim1 ->
                A $ Minion' [OwnedBy opponent, Not victim1] $ \victim2 ->
                    Effect $ ForEachMinion (handleList [victim1, victim2]) $ \victim ->
                        (this `damages` victim) 3


murlocRaider :: MinionCard
murlocRaider = mkMinion Neutral MurlocRaider [Murloc] 1 2 1 []


murlocScout :: MinionCard
murlocScout = uncollectible $ mkMinion Neutral MurlocScout [Murloc] 0 1 1 []


murlocTidehunter :: MinionCard
murlocTidehunter = mkMinion Neutral MurlocTidehunter [Murloc] 2 2 1 [
    Battlecry $ \this ->
        Effect $ (Summon murlocScout) $ RightOf this ]


nightblade :: MinionCard
nightblade = mkMinion Neutral Nightblade [] 5 4 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ (this `damages` opponent) 3 ]


northshireCleric :: MinionCard
northshireCleric = mkMinion Priest NorthshireCleric [] 1 1 3 [
    WheneverMinion $ \this ->
        HealthIsRestored $ \recipient _ ->
            OwnerOf this $ \you ->
                Effect $ when (recipient `Satisfies` [IsMinion]) $ DrawCards you 1 ]


noviceEngineer :: MinionCard
noviceEngineer = mkMinion Neutral NoviceEngineer [] 2 1 1 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


oasisSnapjaw :: MinionCard
oasisSnapjaw = mkMinion Neutral OasisSnapjaw [Beast] 4 2 7 []


ogreMagi :: MinionCard
ogreMagi = mkMinion Neutral OgreMagi [] 4 4 4 [
    SpellDamage 1 ]


polymorph :: SpellCard
polymorph = mkSpell Mage Polymorph 4 $ \_ ->
    A $ Minion' [] $ \target ->
        Effect $ Transform target sheep


powerWordShield :: SpellCard
powerWordShield = mkSpell Priest PowerWordShield 1 $ \this ->
    A $ Minion' [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                Enchant target $ Continuous $ statsDelta 0 2,
                DrawCards you 1 ]


raidLeader :: MinionCard
raidLeader = mkMinion Neutral RaidLeader [] 3 2 2 [
    AuraMinion $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, Not this] $ \minion ->
                Has minion $ statsDelta 1 0 ]


razorfenHunter :: MinionCard
razorfenHunter = mkMinion Neutral RazorfenHunter [] 3 2 3 [
    Battlecry $ \this ->
        Effect $ (Summon boar) $ RightOf this ]


recklessRocketeer :: MinionCard
recklessRocketeer = mkMinion Neutral RecklessRocketeer [] 6 5 2 [
    Charge ]


riverCrocolisk :: MinionCard
riverCrocolisk = mkMinion Neutral RiverCrocolisk [Beast] 2 2 3 []


rockbiterWeapon :: SpellCard
rockbiterWeapon = mkSpell Shaman RockbiterWeapon 1 $ \this ->
    OwnerOf this $ \you ->
        A $ Character' [OwnedBy you] $ \target ->
            Effect $ Enchant target $ Limited $ Until EndOfTurn $ statsDelta 3 0


sacrificialPact :: SpellCard
sacrificialPact = mkSpell Warlock SacrificialPact 0 $ \this ->
    OwnerOf this $ \you ->
        A $ Minion' [HasType Demon] $ \demon ->
            Effect $ Sequence [
                DestroyMinion demon,
                RestoreHealth (PlayerCharacter you) 5 ]


savageRoar :: SpellCard
savageRoar = mkSpell Druid SavageRoar 3 $ \this ->
    OwnerOf this $ \you ->
        All $ Characters [OwnedBy you] $ \friendlies ->
            Effect $ ForEachCharacter friendlies $ \friendly ->
                Enchant friendly $ Limited $ Until EndOfTurn $ statsDelta 2 0


searingTotem :: MinionCard
searingTotem = uncollectible $ mkMinion Shaman SearingTotem [Totem] 1 1 1 []


sen'jinShieldmasta :: MinionCard
sen'jinShieldmasta = mkMinion Neutral Sen'jinShieldmasta [] 4 3 5 [
    Taunt ]


shadowBolt :: SpellCard
shadowBolt = mkSpell Warlock ShadowBolt 3 $ \this ->
    A $ Minion' [] $ \target ->
        Effect $ (this `damages` target) 4


shadowWordDeath :: SpellCard
shadowWordDeath = mkSpell Priest ShadowWordDeath 5 $ \_ ->
    A $ Minion' [RequireMinion (WithAttack GreaterEqual 5)] $ \target ->
        Effect $ DestroyMinion target


shadowWordPain :: SpellCard
shadowWordPain = mkSpell Priest ShadowWordPain 2 $ \_ ->
    A $ Minion' [RequireMinion (WithAttack LessEqual 3)] $ \target ->
        Effect $ DestroyMinion target


shatteredSunCleric :: MinionCard
shatteredSunCleric = mkMinion Neutral ShatteredSunCleric [] 3 3 2 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion' [OwnedBy you] $ \target ->
                Effect $ Enchant target $ Continuous $ statsDelta 1 1 ]


sheep :: MinionCard
sheep = uncollectible $ mkMinion Neutral Sheep [Beast] 0 1 1 []


shieldBlock :: SpellCard
shieldBlock = mkSpell Warrior ShieldBlock 3 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Sequence [
            GainArmor you 5,
            DrawCards you 1 ]


shiv :: SpellCard
shiv = mkSpell Rogue Shiv 2 $ \this ->
    A $ Character' [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 1,
                DrawCards you 1 ]


silverbackPatriarch :: MinionCard
silverbackPatriarch = mkMinion Neutral SilverbackPatriarch [Beast] 3 1 4 [
    Taunt ]


silverHandRecruit :: MinionCard
silverHandRecruit = uncollectible $ mkMinion Paladin SilverHandRecruit [] 1 1 1 []


sinisterStrike :: SpellCard
sinisterStrike = mkSpell Rogue SinisterStrike 1 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ (this `damages` opponent) 3


soulfire :: SpellCard
soulfire = mkSpell Warlock Soulfire 1 $ \this ->
    OwnerOf this $ \you ->
        A $ Character' [] $ \victim ->
            Effect $ Sequence [
                (this `damages` victim) 4,
                DiscardAtRandom you ]


sprint :: SpellCard
sprint = mkSpell Rogue Sprint 7 $ \this ->
    OwnerOf this $ \you ->
        Effect $ DrawCards you 4


starfire :: SpellCard
starfire = mkSpell Druid Starfire 6 $ \this ->
    A $ Character' [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 5,
                DrawCards you 1 ]


stoneclawTotem :: MinionCard
stoneclawTotem = uncollectible $ mkMinion Shaman StoneclawTotem [Totem] 1 0 2 [
    Taunt ]


stonetuskBoar :: MinionCard
stonetuskBoar = mkMinion Neutral StonetuskBoar [Beast] 1 1 1 [
    Charge ]


stormpikeCommando :: MinionCard
stormpikeCommando = mkMinion Neutral StormpikeCommando [] 5 4 2 [
    Battlecry $ \this ->
        A $ Character' [] $ \target ->
            Effect $ (this `damages` target) 2 ]


stormwindKnight :: MinionCard
stormwindKnight = mkMinion Neutral StormwindKnight [] 4 2 5 [
    Charge ]


stormwindChampion :: MinionCard
stormwindChampion = mkMinion Neutral StormwindChampion [] 7 6 6 [
    AuraMinion $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, Not this] $ \minion ->
                Has minion $ statsDelta 1 1 ]


succubus :: MinionCard
succubus = mkMinion Warlock Succubus [Demon] 2 4 3 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DiscardAtRandom you ]


swipe :: SpellCard
swipe = mkSpell Druid Swipe 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            A $ Character' [OwnedBy opponent] $ \target ->
                All $ Characters [OwnedBy opponent, Not target] $ \others ->
                    Effect $ Sequence [
                        (this `damages` target) 4,
                        ForEachCharacter others $ \other ->
                            (this `damages` other) 1 ]


theCoin :: SpellCard
theCoin = uncollectible $ mkSpell Neutral TheCoin 0 $ \this ->
    OwnerOf this $ \you ->
        Effect $ GainManaCrystals you 1 CrystalTemporary


timberWolf :: MinionCard
timberWolf = mkMinion Hunter TimberWolf [Beast] 1 1 1 [
    AuraMinion $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, Not this, HasType Beast] $ \minion ->
                Has minion $ statsDelta 1 0 ]


totemicMight :: SpellCard
totemicMight = mkSpell Shaman TotemicMight 0 $ \this ->
    OwnerOf this $ \you ->
        All $ Minions [OwnedBy you, HasType Totem] $ \totems ->
            Effect $ ForEachMinion totems $ \totem ->
                Enchant totem $ Continuous $ statsDelta 0 2


tundraRhino :: MinionCard
tundraRhino = mkMinion Hunter TundraRhino [Beast] 5 2 5 [
    AuraMinion $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, HasType Beast] $ \minion ->
                HasAbility minion Charge ]


voidwalker :: MinionCard
voidwalker = mkMinion Warlock Voidwalker [Demon] 1 1 3 [
    Taunt ]


voodooDoctor :: MinionCard
voodooDoctor = mkMinion Neutral VoodooDoctor [] 1 2 1 [
    Battlecry $ \_ ->
        A $ Character' [] $ \character ->
            Effect $ RestoreHealth character 2 ]


warGolem :: MinionCard
warGolem = mkMinion Neutral WarGolem [] 7 7 7 []


warsongCommander :: MinionCard
warsongCommander = mkMinion Warrior WarsongCommander [] 3 2 3 [
    AuraMinion $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, HasCharge] $ \minion ->
                Has minion $ statsDelta 1 0 ]


waterElemental :: MinionCard
waterElemental = mkMinion Mage WaterElemental [] 4 3 6 [
    WheneverMinion $ \this ->
        DamageIsDealt $ \victim _ source ->
            Effect $ when (this `Satisfies` [IsDamageSource source]) $ Freeze victim ]


whirlwind :: SpellCard
whirlwind = mkSpell Warrior Whirlwind 1 $ \this ->
    All $ Minions [] $ \minions ->
        Effect $ ForEachMinion minions $ \minion ->
            (this `damages` minion) 1


wickedKnife :: WeaponCard
wickedKnife = uncollectible $ mkWeapon Rogue WickedKnife 1 1 2 []


wildGrowth :: SpellCard
wildGrowth = mkSpell Druid WildGrowth 2 $ \this ->
    OwnerOf this $ \you ->
        Effect $ If (you `Satisfies` [HasMaxManaCrystals])
            (PutInHand you $ CardSpell excessMana)
            $ GainManaCrystals you 1 CrystalEmpty


windfury :: SpellCard
windfury = mkSpell Shaman Basic.Windfury 2 $ \_ ->
    A $ Minion' [] $ \target ->
        Effect $ Enchant target $ Continuous $ Grant Windfury


windspeaker :: MinionCard
windspeaker = mkMinion Shaman Windspeaker [] 4 3 3 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion' [OwnedBy you] $ \target ->
                Effect $ Enchant target $ Continuous $ Grant Windfury ]


wolfRider :: MinionCard
wolfRider = mkMinion Neutral WolfRider [] 3 3 1 [
    Charge ]


wrathOfAirTotem :: MinionCard
wrathOfAirTotem = uncollectible $ mkMinion Shaman WrathOfAirTotem [Totem] 1 0 2 [
    SpellDamage 1 ]








