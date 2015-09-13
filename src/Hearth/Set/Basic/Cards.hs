{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Hearth.Set.Basic.Cards (
    cards,
) where


--------------------------------------------------------------------------------


import Hearth.Authoring.Combinators
import Hearth.CardName
import Hearth.Model
import Hearth.Set.Basic.Names hiding (Charge, Windfury)
import qualified Hearth.Set.Basic.Names as Basic


--------------------------------------------------------------------------------


cards :: (UserConstraint c) => [Card c]
cards = let x = toCard in [
    x ancestralHealing,
    x animalCompanion,
    x arcaneExplosion,
    x arcaneIntellect,
    x arcaneMissiles,
    x arcaneShot,
    x archmage,
    x assassinate,
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
    x waterElemental,
    x warGolem,
    x whirlwind,
    x wildGrowth,
    x windfury,
    x windspeaker,
    x wolfRider,
    x wrathOfAirTotem ]


--------------------------------------------------------------------------------


mkMinion :: (UserConstraint c) => Class -> BasicCardName -> [MinionType] -> Mana -> Attack -> Health -> [Ability c Minion] -> MinionCard c
mkMinion = mkMinion' BasicCardName Free


mkSpell :: (UserConstraint c) => Class -> BasicCardName -> Mana -> SpellEffect c -> SpellCard c
mkSpell = mkSpell' BasicCardName Free


--------------------------------------------------------------------------------


ancestralHealing :: (UserConstraint c) => SpellCard c
ancestralHealing = mkSpell Shaman AncestralHealing 0 $ \_ ->
    A $ Minion [] $ \minion ->
        Effect $ Sequence [
            RestoreToFullHealth $ MinionCharacter minion,
            Enchant minion $ Continuous $ Grant Taunt ]


animalCompanion :: (UserConstraint c) => SpellCard c
animalCompanion = mkSpell Hunter AnimalCompanion 3 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Elect $ Choice $ map (\minion -> Effect $ (you `Summon` minion) Rightmost) [
            huffer,
            leokk,
            misha ]


arcaneExplosion :: (UserConstraint c) => SpellCard c
arcaneExplosion = mkSpell Mage ArcaneExplosion 2 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \enemies ->
                Effect $ ForEach enemies $ \enemy ->
                    (this `damages` enemy) 1


arcaneIntellect :: (UserConstraint c) => SpellCard c
arcaneIntellect = mkSpell Mage ArcaneIntellect 3 $ \this ->
    OwnerOf this $ \you ->
        Effect $ DrawCards you 2


arcaneMissiles :: (UserConstraint c) => SpellCard c
arcaneMissiles = mkSpell Mage ArcaneMissiles 1 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ RandomMissiles [OwnedBy opponent] 3 this


arcaneShot :: (UserConstraint c) => SpellCard c
arcaneShot = mkSpell Hunter ArcaneShot 1 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) 2


archmage :: (UserConstraint c) => MinionCard c
archmage = mkMinion Neutral Archmage [] 6 4 7 [
    SpellDamage 1 ]


assassinate :: (UserConstraint c) => SpellCard c
assassinate = mkSpell Rogue Assassinate 5 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ DestroyMinion target


backstab :: (UserConstraint c) => SpellCard c
backstab = mkSpell Rogue Backstab 0 $ \this ->
    A $ Minion [RequireMinion Undamaged] $ \target ->
        Effect $ (this `damages` target) 2


blessingOfKings :: (UserConstraint c) => SpellCard c
blessingOfKings = mkSpell Paladin BlessingOfKings 4 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ statsDelta 4 4


blessingOfMight :: (UserConstraint c) => SpellCard c
blessingOfMight = mkSpell Paladin BlessingOfMight 4 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ statsDelta 3 0


bloodfenRaptor :: (UserConstraint c) => MinionCard c
bloodfenRaptor = mkMinion Neutral BloodfenRaptor [Beast] 2 3 2 []


bloodlust :: (UserConstraint c) => SpellCard c
bloodlust = mkSpell Shaman Bloodlust 5 $ \this ->
    OwnerOf this $ \you ->
        All $ Minions [OwnedBy you] $ \minions ->
            Effect $ ForEach minions $ \minion ->
                Enchant minion $ Limited $ Until EndOfTurn $ statsDelta 3 0


bluegillWarrior :: (UserConstraint c) => MinionCard c
bluegillWarrior = mkMinion Neutral BluegillWarrior [Murloc] 2 2 1 [
    Charge ]


boar :: (UserConstraint c) => MinionCard c
boar = uncollectible $ mkMinion Neutral Boar [Beast] 1 1 1 []


bootyBayBodyguard :: (UserConstraint c) => MinionCard c
bootyBayBodyguard = mkMinion Neutral BootyBayBodyguard [] 5 5 4 [
    Taunt ]


boulderfistOgre :: (UserConstraint c) => MinionCard c
boulderfistOgre = mkMinion Neutral BoulderfistOgre [] 6 6 7 []


charge :: (UserConstraint c) => SpellCard c
charge = mkSpell Warrior Basic.Charge 3 $ \this ->
    OwnerOf this $ \you ->
        A $ Minion [OwnedBy you] $ \target ->
            Effect $ Sequence [
                Enchant target $ Continuous $ statsDelta 2 0,
                Enchant target $ Continuous $ Grant Charge ]


chillwindYeti :: (UserConstraint c) => MinionCard c
chillwindYeti = mkMinion Neutral ChillwindYeti [] 4 4 5 []


claw :: (UserConstraint c) => SpellCard c
claw = mkSpell Druid Claw 1 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Sequence [
            Enchant you $ Limited $ Until EndOfTurn $ statsDelta 2 0,
            GainArmor you 2 ]


cleave :: (UserConstraint c) => SpellCard c
cleave = mkSpell Warrior Cleave 2 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ Elect $ A $ Minion [OwnedBy opponent] $ \victim1 ->
                A $ Minion [OwnedBy opponent, Not victim1] $ \victim2 ->
                    Effect $ ForEach (handleList [victim1, victim2]) $ \victim ->
                        (this `damages` victim) 2


consecration :: (UserConstraint c) => SpellCard c
consecration = mkSpell Paladin Consecration 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Characters [OwnedBy opponent] $ \enemies ->
                Effect $ ForEach enemies $ \enemy ->
                    (this `damages` enemy) 2


coreHound :: (UserConstraint c) => MinionCard c
coreHound = mkMinion Neutral CoreHound [Beast] 7 9 5 []


corruption :: (UserConstraint c) => SpellCard c
corruption = mkSpell Warlock Corruption 1 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            A $ Minion [OwnedBy opponent] $ \target ->
                Effect $ Enchant target $ Limited $ DelayedEffect (Delay 1 BeginOfTurn) $ DestroyMinion target
    


dalaranMage :: (UserConstraint c) => MinionCard c
dalaranMage = mkMinion Neutral DalaranMage [] 3 1 4 [
    SpellDamage 1 ]


darkscaleHealer :: (UserConstraint c) => MinionCard c
darkscaleHealer = mkMinion Neutral DarkscaleHealer [] 5 4 5 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            All $ Characters [OwnedBy you] $ \friendlies ->
                Effect $ ForEach friendlies $ \friendly ->
                    RestoreHealth friendly 2 ]


deadlyShot :: (UserConstraint c) => SpellCard c
deadlyShot = mkSpell Hunter DeadlyShot 3 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ Elect $ A $ Minion [OwnedBy opponent] $ \victim ->
                Effect $ DestroyMinion victim


divineSpirit :: (UserConstraint c) => SpellCard c
divineSpirit = mkSpell Priest DivineSpirit 2 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ StatsScale 1 2


dragonlingMechanic :: (UserConstraint c) => MinionCard c
dragonlingMechanic = mkMinion Neutral DragonlingMechanic [] 4 2 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ (you `Summon` mechanicalDragonling) $ RightOf this ]


drainLife :: (UserConstraint c) => SpellCard c
drainLife = mkSpell Warlock DrainLife 3 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 2,
                RestoreHealth (PlayerCharacter you) 2 ]


dreadInfernal :: (UserConstraint c) => MinionCard c
dreadInfernal = mkMinion Warlock DreadInfernal [Demon] 6 6 6 [
    Battlecry $ \this ->
        All $ Characters [Not (MinionCharacter this)] $ \victims ->
            Effect $ ForEach victims $ \victim ->
                (this `damages` victim) 1 ]


elvenArcher :: (UserConstraint c) => MinionCard c
elvenArcher = mkMinion Neutral ElvenArcher [] 1 1 1 [
    Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ (this `damages` target) 1 ]


excessMana :: (UserConstraint c) => SpellCard c
excessMana = uncollectible $ mkSpell Druid ExcessMana 0 $ \this ->
    OwnerOf this $ \you ->
        Effect $ DrawCards you 1


execute :: (UserConstraint c) => SpellCard c
execute = mkSpell Warrior Execute 1 $ \_ ->
    A $ Minion [RequireMinion Damaged] $ \target ->
        Effect $ DestroyMinion target


fanOfKnives :: (UserConstraint c) => SpellCard c
fanOfKnives = mkSpell Rogue FanOfKnives 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \enemies ->
                Effect $ Sequence [
                    ForEach enemies $ \enemy ->
                        (this `damages` enemy) 1,
                    DrawCards you 1 ]


fireball :: (UserConstraint c) => SpellCard c
fireball = mkSpell Mage Fireball 4 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) 6


fireElemental :: (UserConstraint c) => MinionCard c
fireElemental = mkMinion Shaman FireElemental [] 6 6 5 [
    Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ (this `damages` target) 3 ]


flamestrike :: (UserConstraint c) => SpellCard c
flamestrike = mkSpell Mage Flamestrike 7 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ ForEach victims $ \victim ->
                    (this `damages` victim) 4


flametongueTotem :: (UserConstraint c) => MinionCard c
flametongueTotem = mkMinion Shaman FlametongueTotem [Totem] 2 0 3 [
    Aura $ \this ->
        EachMinion [AdjacentTo this] $ \minion ->
            Has minion $ statsDelta 2 0 ]


frog :: (UserConstraint c) => MinionCard c
frog = uncollectible $ mkMinion Neutral Frog [Beast] 0 0 1 [
    Taunt ]


frostbolt :: (UserConstraint c) => SpellCard c
frostbolt = mkSpell Mage Frostbolt 2 $ \this ->
    A $ Character [] $ \target ->
        Effect $ Sequence [
            (this `damages` target) 3,
            Freeze target ]


frostNova :: (UserConstraint c) => SpellCard c
frostNova = mkSpell Mage FrostNova 3 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ ForEach victims $ \victim ->
                    Freeze (MinionCharacter victim)


frostShock :: (UserConstraint c) => SpellCard c
frostShock = mkSpell Shaman FrostShock 1 $ \this ->
    A $ Character [] $ \target ->
        Effect $ Sequence [
            (this `damages` target) 1,
            Freeze target ]


frostwolfGrunt :: (UserConstraint c) => MinionCard c
frostwolfGrunt = mkMinion Neutral FrostwolfGrunt [] 2 2 2 [
    Taunt ]


frostwolfWarlord :: (UserConstraint c) => MinionCard c
frostwolfWarlord = mkMinion Neutral FrostwolfWarlord [] 5 4 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            All $ Minions [OwnedBy you, Not this] $ \minions ->
                Effect $ ForEach minions $ \_ ->
                    Enchant this $ Continuous $ statsDelta 1 1 ]


gnomishInventor :: (UserConstraint c) => MinionCard c
gnomishInventor = mkMinion Neutral GnomishInventor [] 4 2 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


goldshireFootman :: (UserConstraint c) => MinionCard c
goldshireFootman = mkMinion Neutral GoldshireFootman [] 1 1 2 [
    Taunt ]


grimscaleOracle :: (UserConstraint c) => MinionCard c
grimscaleOracle = mkMinion Neutral GrimscaleOracle [Murloc] 1 1 1 [
    Aura $ \this ->
        EachMinion [Not this, HasType Murloc] $ \minion ->
            Has minion $ statsDelta 1 0 ]


guardianOfKings :: (UserConstraint c) => MinionCard c
guardianOfKings = mkMinion Paladin GuardianOfKings [] 7 5 6 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ RestoreHealth (PlayerCharacter you) 6 ]


gurubashiBerserker :: (UserConstraint c) => MinionCard c
gurubashiBerserker = mkMinion Neutral GurubashiBerserker [] 5 2 7 [
    Whenever $ \this ->
        DamageIsDealt $ \victim _ _ ->
            Effect $ when (MinionCharacter this `Satisfies` [Is victim]) $ Enchant this $ Continuous $ statsDelta 3 0 ]


hammerOfWrath :: (UserConstraint c) => SpellCard c
hammerOfWrath = mkSpell Paladin HammerOfWrath 4 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 3,
                DrawCards you 1 ]


handOfProtection :: (UserConstraint c) => SpellCard c
handOfProtection = mkSpell Paladin HandOfProtection 1 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ Grant DivineShield


healingTotem :: (UserConstraint c) => MinionCard c
healingTotem = uncollectible $ mkMinion Shaman HealingTotem [Totem] 1 0 2 [
    Whenever $ \this ->
        EndOfTurnEvent $ \player ->
            OwnerOf this $ \you ->
                Effect $ when (player `Satisfies` [Is you]) $ Elect $ All $ Minions [OwnedBy you] $ \minions ->
                    Effect $ ForEach minions $ \minion ->
                        RestoreHealth (MinionCharacter minion) 1 ]


healingTouch :: (UserConstraint c) => SpellCard c
healingTouch = mkSpell Druid HealingTouch 3 $ \_ ->
    A $ Character [] $ \target ->
        Effect $ RestoreHealth target 8


hellfire :: (UserConstraint c) => SpellCard c
hellfire = mkSpell Warlock Hellfire 4 $ \this ->
    All $ Characters [] $ \victims ->
        Effect $ ForEach victims $ \victim ->
            (this `damages` victim) 3


heroicStrike :: (UserConstraint c) => SpellCard c
heroicStrike = mkSpell Warrior HeroicStrike 2 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Enchant you $ Limited $ Until EndOfTurn $ statsDelta 4 0


hex :: (UserConstraint c) => SpellCard c
hex = mkSpell Shaman Hex 3 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Transform target frog


holyLight :: (UserConstraint c) => SpellCard c
holyLight = mkSpell Paladin HolyLight 2 $ \_ ->
    A $ Character [] $ \target ->
        Effect $ RestoreHealth target 6


holyNova :: (UserConstraint c) => SpellCard c
holyNova = mkSpell Priest HolyNova 5 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Characters [OwnedBy you] $ \friendlies ->
                All $ Characters [OwnedBy opponent] $ \enemies ->
                    Effect $ Sequence [
                        ForEach enemies $ \enemy ->
                            (this `damages` enemy) 2,
                        ForEach friendlies $ \friendly ->
                            RestoreHealth friendly 2 ]


holySmite :: (UserConstraint c) => SpellCard c
holySmite = mkSpell Priest HolySmite 1 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) 2


houndmaster :: (UserConstraint c) => MinionCard c
houndmaster = mkMinion Hunter Houndmaster [] 4 4 3 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion [OwnedBy you, HasType Beast] $ \beast ->
                Effect $ Sequence [
                    Enchant beast $ Continuous $ statsDelta 2 2,
                    Enchant beast $ Continuous $ Grant Taunt ]]


huffer :: (UserConstraint c) => MinionCard c
huffer = uncollectible $ mkMinion Hunter Huffer [Beast] 3 4 2 [
    Charge ]


humility :: (UserConstraint c) => SpellCard c
humility = mkSpell Paladin Humility 1 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ ChangeStat (Left 1)


hunter'sMark :: (UserConstraint c) => SpellCard c
hunter'sMark = mkSpell Hunter Hunter'sMark 0 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ ChangeStat (Right 1)


koboldGeomancer :: (UserConstraint c) => MinionCard c
koboldGeomancer = mkMinion Neutral KoboldGeomancer [] 2 2 2 [
    SpellDamage 1 ]


kor'kronElite :: (UserConstraint c) => MinionCard c
kor'kronElite = mkMinion Warrior Kor'kronElite [] 4 4 3 [
    Charge ]


innervate :: (UserConstraint c) => SpellCard c
innervate = mkSpell Druid Innervate 0 $ \this ->
    OwnerOf this $ \you ->
        Effect $ GainManaCrystals you 2 CrystalTemporary


ironbarkProtector :: (UserConstraint c) => MinionCard c
ironbarkProtector = mkMinion Druid IronbarkProtector [] 8 8 8 [
    Taunt ]


ironforgeRifleman :: (UserConstraint c) => MinionCard c
ironforgeRifleman = mkMinion Neutral IronforgeRifleman [] 3 2 2 [
    Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ (this `damages` target) 1 ]


killCommand :: (UserConstraint c) => SpellCard c
killCommand = mkSpell Hunter KillCommand 3 $ \this ->
    OwnerOf this $ \you ->
        A $ Character [] $ \victim -> let
            deal = this `damages` victim
            in Effect $ If (you `Satisfies` [HasMinion [HasType Beast]])
                (deal 5)
                (deal 3)


leokk :: (UserConstraint c) => MinionCard c
leokk = uncollectible $ mkMinion Hunter Leokk [Beast] 3 2 4 [
    Aura $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [Not this, OwnedBy you] $ \minion ->
                Has minion $ statsDelta 1 0 ]


lordOfTheArena :: (UserConstraint c) => MinionCard c
lordOfTheArena = mkMinion Neutral LordOfTheArena [] 6 6 5 [
    Taunt ]


markOfTheWild :: (UserConstraint c) => SpellCard c
markOfTheWild = mkSpell Druid MarkOfTheWild 2 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Sequence [
            Enchant target $ Continuous $ Grant Taunt,
            Enchant target $ Continuous $ statsDelta 2 2 ]


magmaRager :: (UserConstraint c) => MinionCard c
magmaRager = mkMinion Neutral MagmaRager [] 3 5 1 []


mechanicalDragonling :: (UserConstraint c) => MinionCard c
mechanicalDragonling = uncollectible $ mkMinion Neutral MechanicalDragonling [Mech] 1 2 1 []


mindBlast :: (UserConstraint c) => SpellCard c
mindBlast = mkSpell Priest MindBlast 2 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ (this `damages` opponent) 5


mindControl :: (UserConstraint c) => SpellCard c
mindControl = mkSpell Priest MindControl 10 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            A $ Minion [OwnedBy opponent] $ \victim ->
                Effect $ TakeControl you victim


mirrorImage_minion :: (UserConstraint c) => MinionCard c
mirrorImage_minion = uncollectible $ mkMinion Mage MirrorImage_Minion [] 1 0 2 [
    Taunt ]


mirrorImage_spell :: (UserConstraint c) => SpellCard c
mirrorImage_spell = mkSpell Mage MirrorImage_Spell 1 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Sequence $ replicate 2 $ (you `Summon` mirrorImage_minion) Rightmost


misha :: (UserConstraint c) => MinionCard c
misha = uncollectible $ mkMinion Hunter Misha [Beast] 3 4 4 [
    Taunt ]


moonfire :: (UserConstraint c) => SpellCard c
moonfire = mkSpell Druid Moonfire 0 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) 1


-- TODO:
-- MortalCoil hitting a KnifeJuggler juggled to death minion (triggered from say your VioletTeacher)
-- Need to match this behavior - https://www.youtube.com/watch?v=MYGSoWbaIAM
-- Comprehensive explanation - https://www.youtube.com/watch?v=H3d_qlm4Xws
mortalCoil :: (UserConstraint c) => SpellCard c
mortalCoil = mkSpell Warlock MortalCoil 1 $ \this ->
    OwnerOf this $ \you ->
        A $ Minion [] $ \target -> let
            effect = (this `damages` target) 1
            in Effect $ Observing effect $ DamageIsDealt $ \victim _ source -> let
                condition = this `Satisfies` [IsDamageSource source]
                    `And` victim `Satisfies` [WithHealth LessEqual 0]
                in Effect $ when condition $ DrawCards you 1


multiShot :: (UserConstraint c) => SpellCard c
multiShot = mkSpell Hunter MultiShot 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ Elect $ A $ Minion [OwnedBy opponent] $ \victim1 ->
                A $ Minion [OwnedBy opponent, Not victim1] $ \victim2 ->
                    Effect $ ForEach (handleList [victim1, victim2]) $ \victim ->
                        (this `damages` victim) 3


murlocRaider :: (UserConstraint c) => MinionCard c
murlocRaider = mkMinion Neutral MurlocRaider [Murloc] 1 2 1 []


murlocScout :: (UserConstraint c) => MinionCard c
murlocScout = uncollectible $ mkMinion Neutral MurlocScout [Murloc] 0 1 1 []


murlocTidehunter :: (UserConstraint c) => MinionCard c
murlocTidehunter = mkMinion Neutral MurlocTidehunter [Murloc] 2 2 1 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ (you `Summon` murlocScout) $ RightOf this ]


nightblade :: (UserConstraint c) => MinionCard c
nightblade = mkMinion Neutral Nightblade [] 5 4 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ (this `damages` opponent) 3 ]


northshireCleric :: (UserConstraint c) => MinionCard c
northshireCleric = mkMinion Priest NorthshireCleric [] 1 1 3 [
    Whenever $ \this ->
        HealthIsRestored $ \recipient _ ->
            OwnerOf this $ \you ->
                Effect $ when (recipient `Satisfies` [IsMinion]) $ DrawCards you 1 ]


noviceEngineer :: (UserConstraint c) => MinionCard c
noviceEngineer = mkMinion Neutral NoviceEngineer [] 2 1 1 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


oasisSnapjaw :: (UserConstraint c) => MinionCard c
oasisSnapjaw = mkMinion Neutral OasisSnapjaw [Beast] 4 2 7 []


ogreMagi :: (UserConstraint c) => MinionCard c
ogreMagi = mkMinion Neutral OgreMagi [] 4 4 4 [
    SpellDamage 1 ]


polymorph :: (UserConstraint c) => SpellCard c
polymorph = mkSpell Mage Polymorph 4 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Transform target sheep


powerWordShield :: (UserConstraint c) => SpellCard c
powerWordShield = mkSpell Priest PowerWordShield 1 $ \this ->
    A $ Minion [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                Enchant target $ Continuous $ statsDelta 0 2,
                DrawCards you 1 ]


raidLeader :: (UserConstraint c) => MinionCard c
raidLeader = mkMinion Neutral RaidLeader [] 3 2 2 [
    Aura $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, Not this] $ \minion ->
                Has minion $ statsDelta 1 0 ]


razorfenHunter :: (UserConstraint c) => MinionCard c
razorfenHunter = mkMinion Neutral RazorfenHunter [] 3 2 3 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ (you `Summon` boar) $ RightOf this ]


recklessRocketeer :: (UserConstraint c) => MinionCard c
recklessRocketeer = mkMinion Neutral RecklessRocketeer [] 6 5 2 [
    Charge ]


riverCrocolisk :: (UserConstraint c) => MinionCard c
riverCrocolisk = mkMinion Neutral RiverCrocolisk [Beast] 2 2 3 []


rockbiterWeapon :: (UserConstraint c) => SpellCard c
rockbiterWeapon = mkSpell Shaman RockbiterWeapon 1 $ \this ->
    OwnerOf this $ \you ->
        A $ Character [OwnedBy you] $ \target ->
            Effect $ Enchant target $ Limited $ Until EndOfTurn $ statsDelta 3 0


sacrificialPact :: (UserConstraint c) => SpellCard c
sacrificialPact = mkSpell Warlock SacrificialPact 0 $ \this ->
    OwnerOf this $ \you ->
        A $ Minion [HasType Demon] $ \demon ->
            Effect $ Sequence [
                DestroyMinion demon,
                RestoreHealth (PlayerCharacter you) 5 ]


savageRoar :: (UserConstraint c) => SpellCard c
savageRoar = mkSpell Druid SavageRoar 3 $ \this ->
    OwnerOf this $ \you ->
        All $ Characters [OwnedBy you] $ \friendlies ->
            Effect $ ForEach friendlies $ \friendly ->
                Enchant friendly $ Limited $ Until EndOfTurn $ statsDelta 2 0


searingTotem :: (UserConstraint c) => MinionCard c
searingTotem = uncollectible $ mkMinion Shaman SearingTotem [Totem] 1 1 1 []


sen'jinShieldmasta :: (UserConstraint c) => MinionCard c
sen'jinShieldmasta = mkMinion Neutral Sen'jinShieldmasta [] 4 3 5 [
    Taunt ]


shadowBolt :: (UserConstraint c) => SpellCard c
shadowBolt = mkSpell Warlock ShadowBolt 3 $ \this ->
    A $ Minion [] $ \target ->
        Effect $ (this `damages` target) 4


shadowWordDeath :: (UserConstraint c) => SpellCard c
shadowWordDeath = mkSpell Priest ShadowWordDeath 5 $ \_ ->
    A $ Minion [RequireMinion (WithAttack GreaterEqual 5)] $ \target ->
        Effect $ DestroyMinion target


shadowWordPain :: (UserConstraint c) => SpellCard c
shadowWordPain = mkSpell Priest ShadowWordPain 2 $ \_ ->
    A $ Minion [RequireMinion (WithAttack LessEqual 3)] $ \target ->
        Effect $ DestroyMinion target


shatteredSunCleric :: (UserConstraint c) => MinionCard c
shatteredSunCleric = mkMinion Neutral ShatteredSunCleric [] 3 3 2 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion [OwnedBy you, Not this] $ \target ->
                Effect $ Enchant target $ Continuous $ statsDelta 1 1 ]


sheep :: (UserConstraint c) => MinionCard c
sheep = uncollectible $ mkMinion Neutral Sheep [Beast] 0 1 1 []


shieldBlock :: (UserConstraint c) => SpellCard c
shieldBlock = mkSpell Warrior ShieldBlock 3 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Sequence [
            GainArmor you 5,
            DrawCards you 1 ]


shiv :: (UserConstraint c) => SpellCard c
shiv = mkSpell Rogue Shiv 2 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 1,
                DrawCards you 1 ]


silverbackPatriarch :: (UserConstraint c) => MinionCard c
silverbackPatriarch = mkMinion Neutral SilverbackPatriarch [Beast] 3 1 4 [
    Taunt ]


silverHandRecruit :: (UserConstraint c) => MinionCard c
silverHandRecruit = uncollectible $ mkMinion Paladin SilverHandRecruit [] 1 1 1 []


sinisterStrike :: (UserConstraint c) => SpellCard c
sinisterStrike = mkSpell Rogue SinisterStrike 1 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ (this `damages` opponent) 3


soulfire :: (UserConstraint c) => SpellCard c
soulfire = mkSpell Warlock Soulfire 1 $ \this ->
    OwnerOf this $ \you ->
        A $ Character [] $ \victim ->
            Effect $ Sequence [
                (this `damages` victim) 4,
                DiscardAtRandom you ]


sprint :: (UserConstraint c) => SpellCard c
sprint = mkSpell Rogue Sprint 7 $ \this ->
    OwnerOf this $ \you ->
        Effect $ DrawCards you 4


starfire :: (UserConstraint c) => SpellCard c
starfire = mkSpell Druid Starfire 6 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 5,
                DrawCards you 1 ]


stoneclawTotem :: (UserConstraint c) => MinionCard c
stoneclawTotem = uncollectible $ mkMinion Shaman StoneclawTotem [Totem] 1 0 2 [
    Taunt ]


stonetuskBoar :: (UserConstraint c) => MinionCard c
stonetuskBoar = mkMinion Neutral StonetuskBoar [Beast] 1 1 1 [
    Charge ]


stormpikeCommando :: (UserConstraint c) => MinionCard c
stormpikeCommando = mkMinion Neutral StormpikeCommando [] 5 4 2 [
    Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ (this `damages` target) 2 ]


stormwindKnight :: (UserConstraint c) => MinionCard c
stormwindKnight = mkMinion Neutral StormwindKnight [] 4 2 5 [
    Charge ]


stormwindChampion :: (UserConstraint c) => MinionCard c
stormwindChampion = mkMinion Neutral StormwindChampion [] 7 6 6 [
    Aura $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, Not this] $ \minion ->
                Has minion $ statsDelta 1 1 ]


succubus :: (UserConstraint c) => MinionCard c
succubus = mkMinion Warlock Succubus [Demon] 2 4 3 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DiscardAtRandom you ]


swipe :: (UserConstraint c) => SpellCard c
swipe = mkSpell Druid Swipe 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            A $ Character [OwnedBy opponent] $ \target ->
                All $ Characters [OwnedBy opponent, Not target] $ \others ->
                    Effect $ Sequence [
                        (this `damages` target) 4,
                        ForEach others $ \other ->
                            (this `damages` other) 1 ]


theCoin :: (UserConstraint c) => SpellCard c
theCoin = uncollectible $ mkSpell Neutral TheCoin 0 $ \this ->
    OwnerOf this $ \you ->
        Effect $ GainManaCrystals you 1 CrystalTemporary


timberWolf :: (UserConstraint c) => MinionCard c
timberWolf = mkMinion Hunter TimberWolf [Beast] 1 1 1 [
    Aura $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, Not this, HasType Beast] $ \minion ->
                Has minion $ statsDelta 1 0 ]


totemicMight :: (UserConstraint c) => SpellCard c
totemicMight = mkSpell Shaman TotemicMight 0 $ \this ->
    OwnerOf this $ \you ->
        All $ Minions [OwnedBy you, HasType Totem] $ \totems ->
            Effect $ ForEach totems $ \totem ->
                Enchant totem $ Continuous $ statsDelta 0 2


tundraRhino :: (UserConstraint c) => MinionCard c
tundraRhino = mkMinion Hunter TundraRhino [Beast] 5 2 5 [
    Aura $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, HasType Beast] $ \minion ->
                HasAbility minion Charge ]


voidwalker :: (UserConstraint c) => MinionCard c
voidwalker = mkMinion Warlock Voidwalker [Demon] 1 1 3 [
    Taunt ]


voodooDoctor :: (UserConstraint c) => MinionCard c
voodooDoctor = mkMinion Neutral VoodooDoctor [] 1 2 1 [
    Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \character ->
            Effect $ RestoreHealth character 2 ]


waterElemental :: (UserConstraint c) => MinionCard c
waterElemental = mkMinion Mage WaterElemental [] 4 3 6 [
    Whenever $ \this ->
        DamageIsDealt $ \victim _ source ->
            Effect $ when (this `Satisfies` [IsDamageSource source]) $ Freeze victim ]


warGolem :: (UserConstraint c) => MinionCard c
warGolem = mkMinion Neutral WarGolem [] 7 7 7 []


whirlwind :: (UserConstraint c) => SpellCard c
whirlwind = mkSpell Warrior Whirlwind 1 $ \this ->
    All $ Minions [] $ \minions ->
        Effect $ ForEach minions $ \minion ->
            (this `damages` minion) 1


wildGrowth :: (UserConstraint c) => SpellCard c
wildGrowth = mkSpell Druid WildGrowth 2 $ \this ->
    OwnerOf this $ \you ->
        Effect $ If (you `Satisfies` [HasMaxManaCrystals])
            (PutInHand you $ CardSpell excessMana)
            $ GainManaCrystals you 1 CrystalEmpty


windfury :: (UserConstraint c) => SpellCard c
windfury = mkSpell Shaman Basic.Windfury 2 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ Grant Windfury


windspeaker :: (UserConstraint c) => MinionCard c
windspeaker = mkMinion Shaman Windspeaker [] 4 3 3 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion [Not this, OwnedBy you] $ \target ->
                Effect $ Enchant target $ Continuous $ Grant Windfury ]


wolfRider :: (UserConstraint c) => MinionCard c
wolfRider = mkMinion Neutral WolfRider [] 3 3 1 [
    Charge ]


wrathOfAirTotem :: (UserConstraint c) => MinionCard c
wrathOfAirTotem = uncollectible $ mkMinion Shaman WrathOfAirTotem [Totem] 1 0 2 [
    SpellDamage 1 ]








