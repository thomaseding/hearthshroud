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


cards :: (UserConstraint k) => [Card k]
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
    x waterElemental,
    x warGolem,
    x whirlwind,
    x wickedKnife,
    x wildGrowth,
    x windfury,
    x windspeaker,
    x wolfRider,
    x wrathOfAirTotem ]


--------------------------------------------------------------------------------


mkMinion :: (UserConstraint k) => Class -> BasicCardName -> [MinionType] -> Mana -> Attack -> Health -> [Ability k Minion] -> MinionCard k
mkMinion = mkMinion' BasicCardName Free


mkSpell :: (UserConstraint k) => Class -> BasicCardName -> Mana -> SpellEffect k -> SpellCard k
mkSpell = mkSpell' BasicCardName Free


mkWeapon :: (UserConstraint k) => Class -> BasicCardName -> Mana -> Attack -> Durability -> [Ability k Weapon] -> WeaponCard k
mkWeapon = mkWeapon' BasicCardName Free


--------------------------------------------------------------------------------


acidicSwampOoze :: (UserConstraint k) => MinionCard k
acidicSwampOoze = mkMinion Neutral AcidicSwampOoze [] 2 3 2 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                A $ Weapon [OwnedBy opponent] $ \weapon ->
                    Effect $ DestroyWeapon weapon ]


ancestralHealing :: (UserConstraint k) => SpellCard k
ancestralHealing = mkSpell Shaman AncestralHealing 0 $ \_ ->
    A $ Minion [] $ \minion ->
        Effect $ Sequence [
            RestoreToFullHealth $ MinionCharacter minion,
            Enchant minion $ Continuous $ Grant Taunt ]


animalCompanion :: (UserConstraint k) => SpellCard k
animalCompanion = mkSpell Hunter AnimalCompanion 3 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Elect $ Choice $ map (\minion -> Effect $ (you `Summon` minion) Rightmost) [
            huffer,
            leokk,
            misha ]


arcaneExplosion :: (UserConstraint k) => SpellCard k
arcaneExplosion = mkSpell Mage ArcaneExplosion 2 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \enemies ->
                Effect $ ForEach enemies $ \enemy ->
                    (this `damages` enemy) 1


arcaneIntellect :: (UserConstraint k) => SpellCard k
arcaneIntellect = mkSpell Mage ArcaneIntellect 3 $ \this ->
    OwnerOf this $ \you ->
        Effect $ DrawCards you 2


arcaneMissiles :: (UserConstraint k) => SpellCard k
arcaneMissiles = mkSpell Mage ArcaneMissiles 1 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ RandomMissiles [OwnedBy opponent] 3 this


arcaneShot :: (UserConstraint k) => SpellCard k
arcaneShot = mkSpell Hunter ArcaneShot 1 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) 2


arcaniteReaper :: (UserConstraint k) => WeaponCard k
arcaniteReaper = mkWeapon Warrior ArcaniteReaper 5 5 2 []


archmage :: (UserConstraint k) => MinionCard k
archmage = mkMinion Neutral Archmage [] 6 4 7 [
    SpellDamage 1 ]


assassinate :: (UserConstraint k) => SpellCard k
assassinate = mkSpell Rogue Assassinate 5 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ DestroyMinion target


assassin'sBlade :: (UserConstraint k) => WeaponCard k
assassin'sBlade = mkWeapon Rogue Assassin'sBlade 5 3 4 []


backstab :: (UserConstraint k) => SpellCard k
backstab = mkSpell Rogue Backstab 0 $ \this ->
    A $ Minion [RequireMinion Undamaged] $ \target ->
        Effect $ (this `damages` target) 2


blessingOfKings :: (UserConstraint k) => SpellCard k
blessingOfKings = mkSpell Paladin BlessingOfKings 4 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ statsDelta 4 4


blessingOfMight :: (UserConstraint k) => SpellCard k
blessingOfMight = mkSpell Paladin BlessingOfMight 4 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ statsDelta 3 0


bloodfenRaptor :: (UserConstraint k) => MinionCard k
bloodfenRaptor = mkMinion Neutral BloodfenRaptor [Beast] 2 3 2 []


bloodlust :: (UserConstraint k) => SpellCard k
bloodlust = mkSpell Shaman Bloodlust 5 $ \this ->
    OwnerOf this $ \you ->
        All $ Minions [OwnedBy you] $ \minions ->
            Effect $ ForEach minions $ \minion ->
                Enchant minion $ Limited $ Until EndOfTurn $ statsDelta 3 0


bluegillWarrior :: (UserConstraint k) => MinionCard k
bluegillWarrior = mkMinion Neutral BluegillWarrior [Murloc] 2 2 1 [
    Charge ]


boar :: (UserConstraint k) => MinionCard k
boar = uncollectible $ mkMinion Neutral Boar [Beast] 1 1 1 []


bootyBayBodyguard :: (UserConstraint k) => MinionCard k
bootyBayBodyguard = mkMinion Neutral BootyBayBodyguard [] 5 5 4 [
    Taunt ]


boulderfistOgre :: (UserConstraint k) => MinionCard k
boulderfistOgre = mkMinion Neutral BoulderfistOgre [] 6 6 7 []


charge :: (UserConstraint k) => SpellCard k
charge = mkSpell Warrior Basic.Charge 3 $ \this ->
    OwnerOf this $ \you ->
        A $ Minion [OwnedBy you] $ \target ->
            Effect $ Sequence [
                Enchant target $ Continuous $ statsDelta 2 0,
                Enchant target $ Continuous $ Grant Charge ]


chillwindYeti :: (UserConstraint k) => MinionCard k
chillwindYeti = mkMinion Neutral ChillwindYeti [] 4 4 5 []


claw :: (UserConstraint k) => SpellCard k
claw = mkSpell Druid Claw 1 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Sequence [
            Enchant you $ Limited $ Until EndOfTurn $ statsDelta 2 0,
            GainArmor you 2 ]


cleave :: (UserConstraint k) => SpellCard k
cleave = mkSpell Warrior Cleave 2 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ Elect $ A $ Minion [OwnedBy opponent] $ \victim1 ->
                A $ Minion [OwnedBy opponent, Not victim1] $ \victim2 ->
                    Effect $ ForEach (handleList [victim1, victim2]) $ \victim ->
                        (this `damages` victim) 2


consecration :: (UserConstraint k) => SpellCard k
consecration = mkSpell Paladin Consecration 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Characters [OwnedBy opponent] $ \enemies ->
                Effect $ ForEach enemies $ \enemy ->
                    (this `damages` enemy) 2


coreHound :: (UserConstraint k) => MinionCard k
coreHound = mkMinion Neutral CoreHound [Beast] 7 9 5 []


corruption :: (UserConstraint k) => SpellCard k
corruption = mkSpell Warlock Corruption 1 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            A $ Minion [OwnedBy opponent] $ \target ->
                Effect $ Enchant target $ Limited $ DelayedEffect (Delay 1 BeginOfTurn) $ DestroyMinion target
    


dalaranMage :: (UserConstraint k) => MinionCard k
dalaranMage = mkMinion Neutral DalaranMage [] 3 1 4 [
    SpellDamage 1 ]


darkscaleHealer :: (UserConstraint k) => MinionCard k
darkscaleHealer = mkMinion Neutral DarkscaleHealer [] 5 4 5 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            All $ Characters [OwnedBy you] $ \friendlies ->
                Effect $ ForEach friendlies $ \friendly ->
                    RestoreHealth friendly 2 ]


deadlyPoison :: (UserConstraint k) => SpellCard k
deadlyPoison = mkSpell Rogue DeadlyPoison 1 $ \this ->
    OwnerOf this $ \you ->
        A $ Weapon [OwnedBy you] $ \weapon ->
            Effect $ Enchant weapon $ Continuous $ AttackDelta 2


deadlyShot :: (UserConstraint k) => SpellCard k
deadlyShot = mkSpell Hunter DeadlyShot 3 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ Elect $ A $ Minion [OwnedBy opponent] $ \victim ->
                Effect $ DestroyMinion victim


divineSpirit :: (UserConstraint k) => SpellCard k
divineSpirit = mkSpell Priest DivineSpirit 2 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ StatsScale 1 2


dragonlingMechanic :: (UserConstraint k) => MinionCard k
dragonlingMechanic = mkMinion Neutral DragonlingMechanic [] 4 2 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ (you `Summon` mechanicalDragonling) $ RightOf this ]


drainLife :: (UserConstraint k) => SpellCard k
drainLife = mkSpell Warlock DrainLife 3 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 2,
                RestoreHealth (PlayerCharacter you) 2 ]


dreadInfernal :: (UserConstraint k) => MinionCard k
dreadInfernal = mkMinion Warlock DreadInfernal [Demon] 6 6 6 [
    Battlecry $ \this ->
        All $ Characters [Not (MinionCharacter this)] $ \victims ->
            Effect $ ForEach victims $ \victim ->
                (this `damages` victim) 1 ]


elvenArcher :: (UserConstraint k) => MinionCard k
elvenArcher = mkMinion Neutral ElvenArcher [] 1 1 1 [
    Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ (this `damages` target) 1 ]


excessMana :: (UserConstraint k) => SpellCard k
excessMana = uncollectible $ mkSpell Druid ExcessMana 0 $ \this ->
    OwnerOf this $ \you ->
        Effect $ DrawCards you 1


execute :: (UserConstraint k) => SpellCard k
execute = mkSpell Warrior Execute 1 $ \_ ->
    A $ Minion [RequireMinion Damaged] $ \target ->
        Effect $ DestroyMinion target


fanOfKnives :: (UserConstraint k) => SpellCard k
fanOfKnives = mkSpell Rogue FanOfKnives 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \enemies ->
                Effect $ Sequence [
                    ForEach enemies $ \enemy ->
                        (this `damages` enemy) 1,
                    DrawCards you 1 ]


fireball :: (UserConstraint k) => SpellCard k
fireball = mkSpell Mage Fireball 4 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) 6


fireElemental :: (UserConstraint k) => MinionCard k
fireElemental = mkMinion Shaman FireElemental [] 6 6 5 [
    Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ (this `damages` target) 3 ]


flamestrike :: (UserConstraint k) => SpellCard k
flamestrike = mkSpell Mage Flamestrike 7 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ ForEach victims $ \victim ->
                    (this `damages` victim) 4


flametongueTotem :: (UserConstraint k) => MinionCard k
flametongueTotem = mkMinion Shaman FlametongueTotem [Totem] 2 0 3 [
    Aura $ \this ->
        EachMinion [AdjacentTo this] $ \minion ->
            Has minion $ statsDelta 2 0 ]


frog :: (UserConstraint k) => MinionCard k
frog = uncollectible $ mkMinion Neutral Frog [Beast] 0 0 1 [
    Taunt ]


frostbolt :: (UserConstraint k) => SpellCard k
frostbolt = mkSpell Mage Frostbolt 2 $ \this ->
    A $ Character [] $ \target ->
        Effect $ Sequence [
            (this `damages` target) 3,
            Freeze target ]


frostNova :: (UserConstraint k) => SpellCard k
frostNova = mkSpell Mage FrostNova 3 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ ForEach victims $ \victim ->
                    Freeze (MinionCharacter victim)


frostShock :: (UserConstraint k) => SpellCard k
frostShock = mkSpell Shaman FrostShock 1 $ \this ->
    A $ Character [] $ \target ->
        Effect $ Sequence [
            (this `damages` target) 1,
            Freeze target ]


frostwolfGrunt :: (UserConstraint k) => MinionCard k
frostwolfGrunt = mkMinion Neutral FrostwolfGrunt [] 2 2 2 [
    Taunt ]


frostwolfWarlord :: (UserConstraint k) => MinionCard k
frostwolfWarlord = mkMinion Neutral FrostwolfWarlord [] 5 4 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            All $ Minions [OwnedBy you, Not this] $ \minions ->
                Effect $ ForEach minions $ \_ ->
                    Enchant this $ Continuous $ statsDelta 1 1 ]


gnomishInventor :: (UserConstraint k) => MinionCard k
gnomishInventor = mkMinion Neutral GnomishInventor [] 4 2 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


goldshireFootman :: (UserConstraint k) => MinionCard k
goldshireFootman = mkMinion Neutral GoldshireFootman [] 1 1 2 [
    Taunt ]


grimscaleOracle :: (UserConstraint k) => MinionCard k
grimscaleOracle = mkMinion Neutral GrimscaleOracle [Murloc] 1 1 1 [
    Aura $ \this ->
        EachMinion [Not this, HasType Murloc] $ \minion ->
            Has minion $ statsDelta 1 0 ]


guardianOfKings :: (UserConstraint k) => MinionCard k
guardianOfKings = mkMinion Paladin GuardianOfKings [] 7 5 6 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ RestoreHealth (PlayerCharacter you) 6 ]


gurubashiBerserker :: (UserConstraint k) => MinionCard k
gurubashiBerserker = mkMinion Neutral GurubashiBerserker [] 5 2 7 [
    Whenever $ \this ->
        DamageIsDealt $ \victim _ _ ->
            Effect $ when (MinionCharacter this `Satisfies` [Is victim]) $ Enchant this $ Continuous $ statsDelta 3 0 ]


hammerOfWrath :: (UserConstraint k) => SpellCard k
hammerOfWrath = mkSpell Paladin HammerOfWrath 4 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 3,
                DrawCards you 1 ]


handOfProtection :: (UserConstraint k) => SpellCard k
handOfProtection = mkSpell Paladin HandOfProtection 1 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ Grant DivineShield


healingTotem :: (UserConstraint k) => MinionCard k
healingTotem = uncollectible $ mkMinion Shaman HealingTotem [Totem] 1 0 2 [
    Whenever $ \this ->
        EndOfTurnEvent $ \player ->
            OwnerOf this $ \you ->
                Effect $ when (player `Satisfies` [Is you]) $ Elect $ All $ Minions [OwnedBy you] $ \minions ->
                    Effect $ ForEach minions $ \minion ->
                        RestoreHealth (MinionCharacter minion) 1 ]


healingTouch :: (UserConstraint k) => SpellCard k
healingTouch = mkSpell Druid HealingTouch 3 $ \_ ->
    A $ Character [] $ \target ->
        Effect $ RestoreHealth target 8


hellfire :: (UserConstraint k) => SpellCard k
hellfire = mkSpell Warlock Hellfire 4 $ \this ->
    All $ Characters [] $ \victims ->
        Effect $ ForEach victims $ \victim ->
            (this `damages` victim) 3


heroicStrike :: (UserConstraint k) => SpellCard k
heroicStrike = mkSpell Warrior HeroicStrike 2 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Enchant you $ Limited $ Until EndOfTurn $ statsDelta 4 0


hex :: (UserConstraint k) => SpellCard k
hex = mkSpell Shaman Hex 3 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Transform target frog


holyLight :: (UserConstraint k) => SpellCard k
holyLight = mkSpell Paladin HolyLight 2 $ \_ ->
    A $ Character [] $ \target ->
        Effect $ RestoreHealth target 6


holyNova :: (UserConstraint k) => SpellCard k
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


holySmite :: (UserConstraint k) => SpellCard k
holySmite = mkSpell Priest HolySmite 1 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) 2


houndmaster :: (UserConstraint k) => MinionCard k
houndmaster = mkMinion Hunter Houndmaster [] 4 4 3 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion [OwnedBy you, HasType Beast] $ \beast ->
                Effect $ Sequence [
                    Enchant beast $ Continuous $ statsDelta 2 2,
                    Enchant beast $ Continuous $ Grant Taunt ]]


huffer :: (UserConstraint k) => MinionCard k
huffer = uncollectible $ mkMinion Hunter Huffer [Beast] 3 4 2 [
    Charge ]


humility :: (UserConstraint k) => SpellCard k
humility = mkSpell Paladin Humility 1 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ ChangeStat (Left 1)


hunter'sMark :: (UserConstraint k) => SpellCard k
hunter'sMark = mkSpell Hunter Hunter'sMark 0 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ ChangeStat (Right 1)


koboldGeomancer :: (UserConstraint k) => MinionCard k
koboldGeomancer = mkMinion Neutral KoboldGeomancer [] 2 2 2 [
    SpellDamage 1 ]


kor'kronElite :: (UserConstraint k) => MinionCard k
kor'kronElite = mkMinion Warrior Kor'kronElite [] 4 4 3 [
    Charge ]


innervate :: (UserConstraint k) => SpellCard k
innervate = mkSpell Druid Innervate 0 $ \this ->
    OwnerOf this $ \you ->
        Effect $ GainManaCrystals you 2 CrystalTemporary


ironbarkProtector :: (UserConstraint k) => MinionCard k
ironbarkProtector = mkMinion Druid IronbarkProtector [] 8 8 8 [
    Taunt ]


ironforgeRifleman :: (UserConstraint k) => MinionCard k
ironforgeRifleman = mkMinion Neutral IronforgeRifleman [] 3 2 2 [
    Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ (this `damages` target) 1 ]


killCommand :: (UserConstraint k) => SpellCard k
killCommand = mkSpell Hunter KillCommand 3 $ \this ->
    OwnerOf this $ \you ->
        A $ Character [] $ \victim -> let
            deal = this `damages` victim
            in Effect $ If (you `Satisfies` [HasMinion [HasType Beast]])
                (deal 5)
                (deal 3)


leokk :: (UserConstraint k) => MinionCard k
leokk = uncollectible $ mkMinion Hunter Leokk [Beast] 3 2 4 [
    Aura $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [Not this, OwnedBy you] $ \minion ->
                Has minion $ statsDelta 1 0 ]


light'sJustice :: (UserConstraint k) => WeaponCard k
light'sJustice = mkWeapon Paladin Light'sJustice 1 1 4 []


lordOfTheArena :: (UserConstraint k) => MinionCard k
lordOfTheArena = mkMinion Neutral LordOfTheArena [] 6 6 5 [
    Taunt ]


markOfTheWild :: (UserConstraint k) => SpellCard k
markOfTheWild = mkSpell Druid MarkOfTheWild 2 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Sequence [
            Enchant target $ Continuous $ Grant Taunt,
            Enchant target $ Continuous $ statsDelta 2 2 ]


magmaRager :: (UserConstraint k) => MinionCard k
magmaRager = mkMinion Neutral MagmaRager [] 3 5 1 []


mechanicalDragonling :: (UserConstraint k) => MinionCard k
mechanicalDragonling = uncollectible $ mkMinion Neutral MechanicalDragonling [Mech] 1 2 1 []


mindBlast :: (UserConstraint k) => SpellCard k
mindBlast = mkSpell Priest MindBlast 2 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ (this `damages` opponent) 5


mindControl :: (UserConstraint k) => SpellCard k
mindControl = mkSpell Priest MindControl 10 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            A $ Minion [OwnedBy opponent] $ \victim ->
                Effect $ TakeControl you victim


mirrorImage_minion :: (UserConstraint k) => MinionCard k
mirrorImage_minion = uncollectible $ mkMinion Mage MirrorImage_Minion [] 1 0 2 [
    Taunt ]


mirrorImage_spell :: (UserConstraint k) => SpellCard k
mirrorImage_spell = mkSpell Mage MirrorImage_Spell 1 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Sequence $ replicate 2 $ (you `Summon` mirrorImage_minion) Rightmost


misha :: (UserConstraint k) => MinionCard k
misha = uncollectible $ mkMinion Hunter Misha [Beast] 3 4 4 [
    Taunt ]


moonfire :: (UserConstraint k) => SpellCard k
moonfire = mkSpell Druid Moonfire 0 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) 1


-- TODO:
-- MortalCoil hitting a KnifeJuggler juggled to death minion (triggered from say your VioletTeacher)
-- Need to match this behavior - https://www.youtube.com/watch?v=MYGSoWbaIAM
-- Comprehensive explanation - https://www.youtube.com/watch?v=H3d_qlm4Xws
mortalCoil :: (UserConstraint k) => SpellCard k
mortalCoil = mkSpell Warlock MortalCoil 1 $ \this ->
    OwnerOf this $ \you ->
        A $ Minion [] $ \target -> let
            effect = (this `damages` target) 1
            in Effect $ Observing effect $ DamageIsDealt $ \victim _ source -> let
                condition = this `Satisfies` [IsDamageSource source]
                    `And` victim `Satisfies` [WithHealth LessEqual 0]
                in Effect $ when condition $ DrawCards you 1


multiShot :: (UserConstraint k) => SpellCard k
multiShot = mkSpell Hunter MultiShot 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ Elect $ A $ Minion [OwnedBy opponent] $ \victim1 ->
                A $ Minion [OwnedBy opponent, Not victim1] $ \victim2 ->
                    Effect $ ForEach (handleList [victim1, victim2]) $ \victim ->
                        (this `damages` victim) 3


murlocRaider :: (UserConstraint k) => MinionCard k
murlocRaider = mkMinion Neutral MurlocRaider [Murloc] 1 2 1 []


murlocScout :: (UserConstraint k) => MinionCard k
murlocScout = uncollectible $ mkMinion Neutral MurlocScout [Murloc] 0 1 1 []


murlocTidehunter :: (UserConstraint k) => MinionCard k
murlocTidehunter = mkMinion Neutral MurlocTidehunter [Murloc] 2 2 1 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ (you `Summon` murlocScout) $ RightOf this ]


nightblade :: (UserConstraint k) => MinionCard k
nightblade = mkMinion Neutral Nightblade [] 5 4 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ (this `damages` opponent) 3 ]


northshireCleric :: (UserConstraint k) => MinionCard k
northshireCleric = mkMinion Priest NorthshireCleric [] 1 1 3 [
    Whenever $ \this ->
        HealthIsRestored $ \recipient _ ->
            OwnerOf this $ \you ->
                Effect $ when (recipient `Satisfies` [IsMinion]) $ DrawCards you 1 ]


noviceEngineer :: (UserConstraint k) => MinionCard k
noviceEngineer = mkMinion Neutral NoviceEngineer [] 2 1 1 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


oasisSnapjaw :: (UserConstraint k) => MinionCard k
oasisSnapjaw = mkMinion Neutral OasisSnapjaw [Beast] 4 2 7 []


ogreMagi :: (UserConstraint k) => MinionCard k
ogreMagi = mkMinion Neutral OgreMagi [] 4 4 4 [
    SpellDamage 1 ]


polymorph :: (UserConstraint k) => SpellCard k
polymorph = mkSpell Mage Polymorph 4 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Transform target sheep


powerWordShield :: (UserConstraint k) => SpellCard k
powerWordShield = mkSpell Priest PowerWordShield 1 $ \this ->
    A $ Minion [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                Enchant target $ Continuous $ statsDelta 0 2,
                DrawCards you 1 ]


raidLeader :: (UserConstraint k) => MinionCard k
raidLeader = mkMinion Neutral RaidLeader [] 3 2 2 [
    Aura $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, Not this] $ \minion ->
                Has minion $ statsDelta 1 0 ]


razorfenHunter :: (UserConstraint k) => MinionCard k
razorfenHunter = mkMinion Neutral RazorfenHunter [] 3 2 3 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ (you `Summon` boar) $ RightOf this ]


recklessRocketeer :: (UserConstraint k) => MinionCard k
recklessRocketeer = mkMinion Neutral RecklessRocketeer [] 6 5 2 [
    Charge ]


riverCrocolisk :: (UserConstraint k) => MinionCard k
riverCrocolisk = mkMinion Neutral RiverCrocolisk [Beast] 2 2 3 []


rockbiterWeapon :: (UserConstraint k) => SpellCard k
rockbiterWeapon = mkSpell Shaman RockbiterWeapon 1 $ \this ->
    OwnerOf this $ \you ->
        A $ Character [OwnedBy you] $ \target ->
            Effect $ Enchant target $ Limited $ Until EndOfTurn $ statsDelta 3 0


sacrificialPact :: (UserConstraint k) => SpellCard k
sacrificialPact = mkSpell Warlock SacrificialPact 0 $ \this ->
    OwnerOf this $ \you ->
        A $ Minion [HasType Demon] $ \demon ->
            Effect $ Sequence [
                DestroyMinion demon,
                RestoreHealth (PlayerCharacter you) 5 ]


savageRoar :: (UserConstraint k) => SpellCard k
savageRoar = mkSpell Druid SavageRoar 3 $ \this ->
    OwnerOf this $ \you ->
        All $ Characters [OwnedBy you] $ \friendlies ->
            Effect $ ForEach friendlies $ \friendly ->
                Enchant friendly $ Limited $ Until EndOfTurn $ statsDelta 2 0


searingTotem :: (UserConstraint k) => MinionCard k
searingTotem = uncollectible $ mkMinion Shaman SearingTotem [Totem] 1 1 1 []


sen'jinShieldmasta :: (UserConstraint k) => MinionCard k
sen'jinShieldmasta = mkMinion Neutral Sen'jinShieldmasta [] 4 3 5 [
    Taunt ]


shadowBolt :: (UserConstraint k) => SpellCard k
shadowBolt = mkSpell Warlock ShadowBolt 3 $ \this ->
    A $ Minion [] $ \target ->
        Effect $ (this `damages` target) 4


shadowWordDeath :: (UserConstraint k) => SpellCard k
shadowWordDeath = mkSpell Priest ShadowWordDeath 5 $ \_ ->
    A $ Minion [RequireMinion (WithAttack GreaterEqual 5)] $ \target ->
        Effect $ DestroyMinion target


shadowWordPain :: (UserConstraint k) => SpellCard k
shadowWordPain = mkSpell Priest ShadowWordPain 2 $ \_ ->
    A $ Minion [RequireMinion (WithAttack LessEqual 3)] $ \target ->
        Effect $ DestroyMinion target


shatteredSunCleric :: (UserConstraint k) => MinionCard k
shatteredSunCleric = mkMinion Neutral ShatteredSunCleric [] 3 3 2 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion [OwnedBy you, Not this] $ \target ->
                Effect $ Enchant target $ Continuous $ statsDelta 1 1 ]


sheep :: (UserConstraint k) => MinionCard k
sheep = uncollectible $ mkMinion Neutral Sheep [Beast] 0 1 1 []


shieldBlock :: (UserConstraint k) => SpellCard k
shieldBlock = mkSpell Warrior ShieldBlock 3 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Sequence [
            GainArmor you 5,
            DrawCards you 1 ]


shiv :: (UserConstraint k) => SpellCard k
shiv = mkSpell Rogue Shiv 2 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 1,
                DrawCards you 1 ]


silverbackPatriarch :: (UserConstraint k) => MinionCard k
silverbackPatriarch = mkMinion Neutral SilverbackPatriarch [Beast] 3 1 4 [
    Taunt ]


silverHandRecruit :: (UserConstraint k) => MinionCard k
silverHandRecruit = uncollectible $ mkMinion Paladin SilverHandRecruit [] 1 1 1 []


sinisterStrike :: (UserConstraint k) => SpellCard k
sinisterStrike = mkSpell Rogue SinisterStrike 1 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ (this `damages` opponent) 3


soulfire :: (UserConstraint k) => SpellCard k
soulfire = mkSpell Warlock Soulfire 1 $ \this ->
    OwnerOf this $ \you ->
        A $ Character [] $ \victim ->
            Effect $ Sequence [
                (this `damages` victim) 4,
                DiscardAtRandom you ]


sprint :: (UserConstraint k) => SpellCard k
sprint = mkSpell Rogue Sprint 7 $ \this ->
    OwnerOf this $ \you ->
        Effect $ DrawCards you 4


starfire :: (UserConstraint k) => SpellCard k
starfire = mkSpell Druid Starfire 6 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 5,
                DrawCards you 1 ]


stoneclawTotem :: (UserConstraint k) => MinionCard k
stoneclawTotem = uncollectible $ mkMinion Shaman StoneclawTotem [Totem] 1 0 2 [
    Taunt ]


stonetuskBoar :: (UserConstraint k) => MinionCard k
stonetuskBoar = mkMinion Neutral StonetuskBoar [Beast] 1 1 1 [
    Charge ]


stormpikeCommando :: (UserConstraint k) => MinionCard k
stormpikeCommando = mkMinion Neutral StormpikeCommando [] 5 4 2 [
    Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ (this `damages` target) 2 ]


stormwindKnight :: (UserConstraint k) => MinionCard k
stormwindKnight = mkMinion Neutral StormwindKnight [] 4 2 5 [
    Charge ]


stormwindChampion :: (UserConstraint k) => MinionCard k
stormwindChampion = mkMinion Neutral StormwindChampion [] 7 6 6 [
    Aura $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, Not this] $ \minion ->
                Has minion $ statsDelta 1 1 ]


succubus :: (UserConstraint k) => MinionCard k
succubus = mkMinion Warlock Succubus [Demon] 2 4 3 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DiscardAtRandom you ]


swipe :: (UserConstraint k) => SpellCard k
swipe = mkSpell Druid Swipe 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            A $ Character [OwnedBy opponent] $ \target ->
                All $ Characters [OwnedBy opponent, Not target] $ \others ->
                    Effect $ Sequence [
                        (this `damages` target) 4,
                        ForEach others $ \other ->
                            (this `damages` other) 1 ]


theCoin :: (UserConstraint k) => SpellCard k
theCoin = uncollectible $ mkSpell Neutral TheCoin 0 $ \this ->
    OwnerOf this $ \you ->
        Effect $ GainManaCrystals you 1 CrystalTemporary


timberWolf :: (UserConstraint k) => MinionCard k
timberWolf = mkMinion Hunter TimberWolf [Beast] 1 1 1 [
    Aura $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, Not this, HasType Beast] $ \minion ->
                Has minion $ statsDelta 1 0 ]


totemicMight :: (UserConstraint k) => SpellCard k
totemicMight = mkSpell Shaman TotemicMight 0 $ \this ->
    OwnerOf this $ \you ->
        All $ Minions [OwnedBy you, HasType Totem] $ \totems ->
            Effect $ ForEach totems $ \totem ->
                Enchant totem $ Continuous $ statsDelta 0 2


tundraRhino :: (UserConstraint k) => MinionCard k
tundraRhino = mkMinion Hunter TundraRhino [Beast] 5 2 5 [
    Aura $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, HasType Beast] $ \minion ->
                HasAbility minion Charge ]


voidwalker :: (UserConstraint k) => MinionCard k
voidwalker = mkMinion Warlock Voidwalker [Demon] 1 1 3 [
    Taunt ]


voodooDoctor :: (UserConstraint k) => MinionCard k
voodooDoctor = mkMinion Neutral VoodooDoctor [] 1 2 1 [
    Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \character ->
            Effect $ RestoreHealth character 2 ]


waterElemental :: (UserConstraint k) => MinionCard k
waterElemental = mkMinion Mage WaterElemental [] 4 3 6 [
    Whenever $ \this ->
        DamageIsDealt $ \victim _ source ->
            Effect $ when (this `Satisfies` [IsDamageSource source]) $ Freeze victim ]


warGolem :: (UserConstraint k) => MinionCard k
warGolem = mkMinion Neutral WarGolem [] 7 7 7 []


whirlwind :: (UserConstraint k) => SpellCard k
whirlwind = mkSpell Warrior Whirlwind 1 $ \this ->
    All $ Minions [] $ \minions ->
        Effect $ ForEach minions $ \minion ->
            (this `damages` minion) 1


wickedKnife :: (UserConstraint k) => WeaponCard k
wickedKnife = uncollectible $ mkWeapon Rogue WickedKnife 1 1 2 []


wildGrowth :: (UserConstraint k) => SpellCard k
wildGrowth = mkSpell Druid WildGrowth 2 $ \this ->
    OwnerOf this $ \you ->
        Effect $ If (you `Satisfies` [HasMaxManaCrystals])
            (PutInHand you $ CardSpell excessMana)
            $ GainManaCrystals you 1 CrystalEmpty


windfury :: (UserConstraint k) => SpellCard k
windfury = mkSpell Shaman Basic.Windfury 2 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ Grant Windfury


windspeaker :: (UserConstraint k) => MinionCard k
windspeaker = mkMinion Shaman Windspeaker [] 4 3 3 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion [Not this, OwnedBy you] $ \target ->
                Effect $ Enchant target $ Continuous $ Grant Windfury ]


wolfRider :: (UserConstraint k) => MinionCard k
wolfRider = mkMinion Neutral WolfRider [] 3 3 1 [
    Charge ]


wrathOfAirTotem :: (UserConstraint k) => MinionCard k
wrathOfAirTotem = uncollectible $ mkMinion Shaman WrathOfAirTotem [Totem] 1 0 2 [
    SpellDamage 1 ]








