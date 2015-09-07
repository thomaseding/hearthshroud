{-# LANGUAGE NoMonomorphismRestriction #-}


module Hearth.Set.Basic.Cards (
    cards,
) where


--------------------------------------------------------------------------------


import Hearth.Authoring.Combinators
import Hearth.CardName
import Hearth.Model
import Hearth.Set.Basic.Names hiding (Charge)
import qualified Hearth.Set.Basic.Names as Basic


--------------------------------------------------------------------------------


cards :: [Card]
cards = let x = toCard in [
    x animalCompanion,
    x arcaneExplosion,
    x arcaneIntellect,
    x arcaneShot,
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
    x huffer,
    x humility,
    x hunter'sMark,
    x ironbarkProtector,
    x innervate,
    x ironforgeRifleman,
    x kor'kronElite,
    x leokk,
    x lordOfTheArena,
    x magmaRager,
    x markOfTheWild,
    x mechanicalDragonling,
    x mindBlast,
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
    x polymorph,
    x powerWordShield,
    x raidLeader,
    x razorfenHunter,
    x recklessRocketeer,
    x riverCrocolisk,
    x rockbiterWeapon,
    x savageRoar,
    x searingTotem,
    x sen'jinShieldmasta,
    x shadowBolt,
    x shadowWordDeath,
    x shadowWordPain,
    x shatteredSunCleric,
    x sheep,
    x shiv,
    x silverbackPatriarch,
    x sinisterStrike,
    x sprint,
    x starfire,
    x stoneclawTotem,
    x stonetuskBoar,
    x stormpikeCommando,
    x stormwindChampion,
    x stormwindKnight,
    x swipe,
    x theCoin,
    x voidwalker,
    x voodooDoctor,
    x waterElemental,
    x warGolem,
    x whirlwind,
    x wildGrowth,
    x wolfRider ]


--------------------------------------------------------------------------------


mkMinion :: Class -> BasicCardName -> [MinionType] -> Mana -> Attack -> Health -> [Ability] -> Minion
mkMinion = mkMinion' BasicCardName Free


mkSpell :: Class -> BasicCardName -> Mana -> SpellEffect -> Spell
mkSpell = mkSpell' BasicCardName Free


--------------------------------------------------------------------------------


animalCompanion :: Spell
animalCompanion = mkSpell Hunter AnimalCompanion 3 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Elect $ Choice $ map Effect [
            (you `Summon` huffer) Rightmost,
            (you `Summon` leokk) Rightmost,
            (you `Summon` misha) Rightmost ]


arcaneExplosion :: Spell
arcaneExplosion = mkSpell Mage ArcaneExplosion 2 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \enemies ->
                Effect $ ForEach enemies $ \enemy ->
                    (this `damages` enemy) 1


arcaneIntellect :: Spell
arcaneIntellect = mkSpell Mage ArcaneIntellect 3 $ \this ->
    OwnerOf this $ \you ->
        Effect $ DrawCards you 2


arcaneShot :: Spell
arcaneShot = mkSpell Hunter ArcaneShot 1 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) 2


assassinate :: Spell
assassinate = mkSpell Rogue Assassinate 5 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ DestroyMinion target


backstab :: Spell
backstab = mkSpell Rogue Backstab 0 $ \this ->
    A $ Minion [RequireMinion Undamaged] $ \target ->
        Effect $ (this `damages` target) 2


blessingOfKings :: Spell
blessingOfKings = mkSpell Paladin BlessingOfKings 4 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ statsDelta 4 4


blessingOfMight :: Spell
blessingOfMight = mkSpell Paladin BlessingOfMight 4 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ statsDelta 3 0


bloodfenRaptor :: Minion
bloodfenRaptor = mkMinion Neutral BloodfenRaptor [Beast] 2 3 2 []


bloodlust :: Spell
bloodlust = mkSpell Shaman Bloodlust 5 $ \this ->
    OwnerOf this $ \you ->
        All $ Minions [OwnedBy you] $ \minions ->
            Effect $ ForEach minions $ \minion ->
                Enchant minion $ Limited $ Until EndOfTurn $ statsDelta 3 0


bluegillWarrior :: Minion
bluegillWarrior = mkMinion Neutral BluegillWarrior [Murloc] 2 2 1 [
    Charge ]


boar :: Minion
boar = uncollectible $ mkMinion Neutral Boar [Beast] 1 1 1 []


bootyBayBodyguard :: Minion
bootyBayBodyguard = mkMinion Neutral BootyBayBodyguard [] 5 5 4 [
    Taunt ]


boulderfistOgre :: Minion
boulderfistOgre = mkMinion Neutral BoulderfistOgre [] 6 6 7 []


charge :: Spell
charge = mkSpell Warrior Basic.Charge 3 $ \this ->
    OwnerOf this $ \you ->
        A $ Minion [OwnedBy you] $ \target ->
            Effect $ Sequence [
                Enchant target $ Continuous $ statsDelta 2 0,
                GrantAbilities target [
                    Charge ]]


chillwindYeti :: Minion
chillwindYeti = mkMinion Neutral ChillwindYeti [] 4 4 5 []


claw :: Spell
claw = mkSpell Druid Claw 1 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Sequence [
            Enchant you $ Limited $ Until EndOfTurn $ statsDelta 2 0,
            GainArmor you 2 ]


cleave :: Spell
cleave = mkSpell Warrior Cleave 2 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ Elect $ A $ Minion [OwnedBy opponent] $ \victim1 ->
                A $ Minion [OwnedBy opponent, Not victim1] $ \victim2 ->
                    Effect $ ForEach (handleList [victim1, victim2]) $ \victim ->
                        (this `damages` victim) 2


consecration :: Spell
consecration = mkSpell Paladin Consecration 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Characters [OwnedBy opponent] $ \enemies ->
                Effect $ ForEach enemies $ \enemy ->
                    (this `damages` enemy) 2


coreHound :: Minion
coreHound = mkMinion Neutral CoreHound [Beast] 7 9 5 []


corruption :: Spell
corruption = mkSpell Warlock Corruption 1 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            A $ Minion [OwnedBy opponent] $ \target ->
                Effect $ Enchant target $ Limited $ DelayedEffect (Delay 1 BeginOfTurn) $ DestroyMinion target
    


darkscaleHealer :: Minion
darkscaleHealer = mkMinion Neutral DarkscaleHealer [] 5 4 5 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            All $ Characters [OwnedBy you] $ \friendlies ->
                Effect $ ForEach friendlies $ \friendly ->
                    RestoreHealth friendly 2 ]


deadlyShot :: Spell
deadlyShot = mkSpell Hunter DeadlyShot 3 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ Elect $ A $ Minion [OwnedBy opponent] $ \victim ->
                Effect $ DestroyMinion victim


divineSpirit :: Spell
divineSpirit = mkSpell Priest DivineSpirit 2 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ StatsScale 1 2


dragonlingMechanic :: Minion
dragonlingMechanic = mkMinion Neutral DragonlingMechanic [] 4 2 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ (you `Summon` mechanicalDragonling) $ RightOf this ]


drainLife :: Spell
drainLife = mkSpell Warlock DrainLife 3 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 2,
                RestoreHealth (PlayerCharacter you) 2 ]


dreadInfernal :: Minion
dreadInfernal = mkMinion Warlock DreadInfernal [Demon] 6 6 6 [
    Battlecry $ \this ->
        All $ Characters [Not (MinionCharacter this)] $ \victims ->
            Effect $ ForEach victims $ \victim ->
                (this `damages` victim) 1 ]


elvenArcher :: Minion
elvenArcher = mkMinion Neutral ElvenArcher [] 1 1 1 [
    Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ (this `damages` target) 1 ]


excessMana :: Spell
excessMana = uncollectible $ mkSpell Druid ExcessMana 0 $ \this ->
    OwnerOf this $ \you ->
        Effect $ DrawCards you 1


execute :: Spell
execute = mkSpell Warrior Execute 1 $ \_ ->
    A $ Minion [RequireMinion Damaged] $ \target ->
        Effect $ DestroyMinion target


fanOfKnives :: Spell
fanOfKnives = mkSpell Rogue FanOfKnives 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \enemies ->
                Effect $ Sequence [
                    ForEach enemies $ \enemy ->
                        (this `damages` enemy) 1,
                    DrawCards you 1 ]


fireball :: Spell
fireball = mkSpell Mage Fireball 4 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) 6


fireElemental :: Minion
fireElemental = mkMinion Shaman FireElemental [] 6 6 5 [
    Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ (this `damages` target) 3 ]


flamestrike :: Spell
flamestrike = mkSpell Mage Flamestrike 7 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ ForEach victims $ \victim ->
                    (this `damages` victim) 4


flametongueTotem :: Minion
flametongueTotem = mkMinion Shaman FlametongueTotem [Totem] 2 0 3 [
    Aura $ \this ->
        EachMinion [AdjacentTo this] $ \minion ->
            Has minion $ statsDelta 2 0 ]


frog :: Minion
frog = uncollectible $ mkMinion Neutral Frog [Beast] 0 0 1 [
    Taunt ]


frostbolt :: Spell
frostbolt = mkSpell Mage Frostbolt 2 $ \this ->
    A $ Character [] $ \target ->
        Effect $ Sequence [
            (this `damages` target) 3,
            Freeze target ]


frostNova :: Spell
frostNova = mkSpell Mage FrostNova 3 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ ForEach victims $ \victim ->
                    Freeze (MinionCharacter victim)


frostShock :: Spell
frostShock = mkSpell Shaman FrostShock 1 $ \this ->
    A $ Character [] $ \target ->
        Effect $ Sequence [
            (this `damages` target) 1,
            Freeze target ]


frostwolfGrunt :: Minion
frostwolfGrunt = mkMinion Neutral FrostwolfGrunt [] 2 2 2 [
    Taunt ]


frostwolfWarlord :: Minion
frostwolfWarlord = mkMinion Neutral FrostwolfWarlord [] 5 4 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            All $ Minions [OwnedBy you, Not this] $ \minions -> do
                Effect $ ForEach minions $ \_ ->
                    Enchant this $ Continuous $ statsDelta 1 1 ]


gnomishInventor :: Minion
gnomishInventor = mkMinion Neutral GnomishInventor [] 4 2 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


goldshireFootman :: Minion
goldshireFootman = mkMinion Neutral GoldshireFootman [] 1 1 2 [
    Taunt ]


guardianOfKings :: Minion
guardianOfKings = mkMinion Paladin GuardianOfKings [] 7 5 6 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ RestoreHealth (PlayerCharacter you) 6 ]


gurubashiBerserker :: Minion
gurubashiBerserker = mkMinion Neutral GurubashiBerserker [] 5 2 7 [
    Whenever $ \this ->
        DamageIsDealt $ \victim _ _ ->
            Effect $ when (MinionCharacter this `Satisfies` [Is victim]) $ Enchant this $ Continuous $ statsDelta 3 0 ]


hammerOfWrath :: Spell
hammerOfWrath = mkSpell Paladin HammerOfWrath 4 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 3,
                DrawCards you 1 ]


handOfProtection :: Spell
handOfProtection = mkSpell Paladin HandOfProtection 1 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ GrantAbilities target [
            DivineShield ]


healingTotem :: Minion
healingTotem = uncollectible $ mkMinion Shaman HealingTotem [Totem] 1 0 2 [
    Whenever $ \this ->
        EndOfTurnEvent $ \player ->
            OwnerOf this $ \you ->
                Effect $ when (player `Satisfies` [Is you]) $ Elect $ All $ Minions [OwnedBy you] $ \minions ->
                    Effect $ ForEach minions $ \minion ->
                        RestoreHealth (MinionCharacter minion) 1 ]


healingTouch :: Spell
healingTouch = mkSpell Druid HealingTouch 3 $ \_ ->
    A $ Character [] $ \target ->
        Effect $ RestoreHealth target 8


hellfire :: Spell
hellfire = mkSpell Warlock Hellfire 4 $ \this ->
    All $ Characters [] $ \victims ->
        Effect $ ForEach victims $ \victim ->
            (this `damages` victim) 3


heroicStrike :: Spell
heroicStrike = mkSpell Warrior HeroicStrike 2 $ \this ->
    OwnerOf this $ \you ->
        Effect $ Enchant you $ Limited $ Until EndOfTurn $ statsDelta 4 0


hex :: Spell
hex = mkSpell Shaman Hex 3 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Transform target frog


holyLight :: Spell
holyLight = mkSpell Paladin HolyLight 2 $ \_ ->
    A $ Character [] $ \target ->
        Effect $ RestoreHealth target 6


holyNova :: Spell
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


holySmite :: Spell
holySmite = mkSpell Priest HolySmite 1 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) 2


huffer :: Minion
huffer = uncollectible $ mkMinion Hunter Huffer [Beast] 3 4 2 [
    Charge ]


humility :: Spell
humility = mkSpell Paladin Humility 1 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ ChangeStat (Left 1)


hunter'sMark :: Spell
hunter'sMark = mkSpell Hunter Hunter'sMark 0 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ ChangeStat (Right 1)


kor'kronElite :: Minion
kor'kronElite = mkMinion Warrior Kor'kronElite [] 4 4 3 [
    Charge ]


innervate :: Spell
innervate = mkSpell Druid Innervate 0 $ \this ->
    OwnerOf this $ \you ->
        Effect $ GainManaCrystals you 2 CrystalTemporary


ironbarkProtector :: Minion
ironbarkProtector = mkMinion Druid IronbarkProtector [] 8 8 8 [
    Taunt ]


ironforgeRifleman :: Minion
ironforgeRifleman = mkMinion Neutral IronforgeRifleman [] 3 2 2 [
    Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ (this `damages` target) 1 ]


leokk :: Minion
leokk = uncollectible $ mkMinion Hunter Leokk [Beast] 3 2 4 [
    Aura $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [Not this, OwnedBy you] $ \minion ->
                Has minion $ statsDelta 1 0 ]


lordOfTheArena :: Minion
lordOfTheArena = mkMinion Neutral LordOfTheArena [] 6 6 5 [
    Taunt ]


markOfTheWild :: Spell
markOfTheWild = mkSpell Druid MarkOfTheWild 2 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Sequence [
            GrantAbilities target [
                Taunt ],
            Enchant target $ Continuous $ statsDelta 2 2 ]


magmaRager :: Minion
magmaRager = mkMinion Neutral MagmaRager [] 3 5 1 []


mechanicalDragonling :: Minion
mechanicalDragonling = uncollectible $ mkMinion Neutral MechanicalDragonling [Mech] 1 2 1 []


mindBlast :: Spell
mindBlast = mkSpell Priest MindBlast 2 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ (this `damages` opponent) 5


misha :: Minion
misha = uncollectible $ mkMinion Hunter Misha [Beast] 3 4 4 [
    Taunt ]


moonfire :: Spell
moonfire = mkSpell Druid Moonfire 0 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) 1


-- TODO:
-- MortalCoil hitting a KnifeJuggler juggled to death minion (triggered from say your VioletTeacher)
-- Need to match this behavior - https://www.youtube.com/watch?v=MYGSoWbaIAM
-- Comprehensive explanation - https://www.youtube.com/watch?v=H3d_qlm4Xws
mortalCoil :: Spell
mortalCoil = mkSpell Warlock MortalCoil 1 $ \this ->
    OwnerOf this $ \you ->
        A $ Minion [] $ \target -> let
            effect = (this `damages` target) 1
            in Effect $ Observing effect $ DamageIsDealt $ \victim _ source -> let
                condition = this `Satisfies` [IsDamageSource source]
                    `And` victim `Satisfies` [WithHealth LessEqual 0]
                in Effect $ when condition $ DrawCards you 1


multiShot :: Spell
multiShot = mkSpell Hunter MultiShot 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ Elect $ A $ Minion [OwnedBy opponent] $ \victim1 ->
                A $ Minion [OwnedBy opponent, Not victim1] $ \victim2 ->
                    Effect $ ForEach (handleList [victim1, victim2]) $ \victim ->
                        (this `damages` victim) 3


murlocRaider :: Minion
murlocRaider = mkMinion Neutral MurlocRaider [Murloc] 1 2 1 []


murlocScout :: Minion
murlocScout = uncollectible $ mkMinion Neutral MurlocScout [Murloc] 0 1 1 []


murlocTidehunter :: Minion
murlocTidehunter = mkMinion Neutral MurlocTidehunter [Murloc] 2 2 1 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ (you `Summon` murlocScout) $ RightOf this ]


nightblade :: Minion
nightblade = mkMinion Neutral Nightblade [] 5 4 4 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ (this `damages` opponent) 3 ]


northshireCleric :: Minion
northshireCleric = mkMinion Priest NorthshireCleric [] 1 1 3 [
    Whenever $ \this ->
        HealthIsRestored $ \recipient _ ->
            OwnerOf this $ \you ->
                Effect $ when (recipient `Satisfies` [IsMinion]) $ DrawCards you 1 ]


noviceEngineer :: Minion
noviceEngineer = mkMinion Neutral NoviceEngineer [] 2 1 1 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


oasisSnapjaw :: Minion
oasisSnapjaw = mkMinion Neutral OasisSnapjaw [Beast] 4 2 7 []


polymorph :: Spell
polymorph = mkSpell Mage Polymorph 4 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Transform target sheep


powerWordShield :: Spell
powerWordShield = mkSpell Priest PowerWordShield 1 $ \this ->
    A $ Minion [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                Enchant target $ Continuous $ statsDelta 0 2,
                DrawCards you 1 ]


raidLeader :: Minion
raidLeader = mkMinion Neutral RaidLeader [] 3 2 2 [
    Aura $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, Not this] $ \minion ->
                Has minion $ statsDelta 1 0 ]


razorfenHunter :: Minion
razorfenHunter = mkMinion Neutral RazorfenHunter [] 3 2 3 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ (you `Summon` boar) $ RightOf this ]


recklessRocketeer :: Minion
recklessRocketeer = mkMinion Neutral RecklessRocketeer [] 6 5 2 [
    Charge ]


riverCrocolisk :: Minion
riverCrocolisk = mkMinion Neutral RiverCrocolisk [Beast] 2 2 3 []


rockbiterWeapon :: Spell
rockbiterWeapon = mkSpell Shaman RockbiterWeapon 1 $ \this ->
    OwnerOf this $ \you ->
        A $ Character [OwnedBy you] $ \target ->
            Effect $ Enchant target $ Limited $ Until EndOfTurn $ statsDelta 3 0


savageRoar :: Spell
savageRoar = mkSpell Druid SavageRoar 3 $ \this ->
    OwnerOf this $ \you ->
        All $ Characters [OwnedBy you] $ \friendlies ->
            Effect $ ForEach friendlies $ \friendly ->
                Enchant friendly $ Limited $ Until EndOfTurn $ statsDelta 2 0


searingTotem :: Minion
searingTotem = uncollectible $ mkMinion Shaman SearingTotem [Totem] 1 1 1 []


sen'jinShieldmasta :: Minion
sen'jinShieldmasta = mkMinion Neutral Sen'jinShieldmasta [] 4 3 5 [
    Taunt ]


shadowBolt :: Spell
shadowBolt = mkSpell Warlock ShadowBolt 3 $ \this ->
    A $ Minion [] $ \target ->
        Effect $ (this `damages` target) 4


shadowWordDeath :: Spell
shadowWordDeath = mkSpell Priest ShadowWordDeath 5 $ \_ ->
    A $ Minion [RequireMinion (WithAttack GreaterEqual 5)] $ \target ->
        Effect $ DestroyMinion target


shadowWordPain :: Spell
shadowWordPain = mkSpell Priest ShadowWordPain 2 $ \_ ->
    A $ Minion [RequireMinion (WithAttack LessEqual 3)] $ \target ->
        Effect $ DestroyMinion target


shatteredSunCleric :: Minion
shatteredSunCleric = mkMinion Neutral ShatteredSunCleric [] 3 3 2 [
    Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion [OwnedBy you, Not this] $ \target ->
                Effect $ Enchant target $ Continuous $ statsDelta 1 1 ]


sheep :: Minion
sheep = uncollectible $ mkMinion Neutral Sheep [Beast] 0 1 1 []


shiv :: Spell
shiv = mkSpell Rogue Shiv 2 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 1,
                DrawCards you 1 ]


silverbackPatriarch :: Minion
silverbackPatriarch = mkMinion Neutral SilverbackPatriarch [Beast] 3 1 4 [
    Taunt ]


sinisterStrike :: Spell
sinisterStrike = mkSpell Rogue SinisterStrike 1 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ (this `damages` opponent) 3


sprint :: Spell
sprint = mkSpell Rogue Sprint 7 $ \this ->
    OwnerOf this $ \you ->
        Effect $ DrawCards you 4


starfire :: Spell
starfire = mkSpell Druid Starfire 6 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 5,
                DrawCards you 1 ]


stoneclawTotem :: Minion
stoneclawTotem = uncollectible $ mkMinion Shaman StoneclawTotem [Totem] 1 0 2 [
    Taunt ]


stonetuskBoar :: Minion
stonetuskBoar = mkMinion Neutral StonetuskBoar [Beast] 1 1 1 [
    Charge ]


stormpikeCommando :: Minion
stormpikeCommando = mkMinion Neutral StormpikeCommando [] 5 4 2 [
    Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ (this `damages` target) 2 ]


stormwindKnight :: Minion
stormwindKnight = mkMinion Neutral StormwindKnight [] 4 2 5 [
    Charge ]


stormwindChampion :: Minion
stormwindChampion = mkMinion Neutral StormwindChampion [] 7 6 6 [
    Aura $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, Not this] $ \minion ->
                Has minion $ statsDelta 1 1 ]


swipe :: Spell
swipe = mkSpell Druid Swipe 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            A $ Character [OwnedBy opponent] $ \target ->
                All $ Characters [OwnedBy opponent, Not target] $ \others ->
                    Effect $ Sequence [
                        (this `damages` target) 4,
                        ForEach others $ \other ->
                            (this `damages` other) 1 ]


theCoin :: Spell
theCoin = uncollectible $ mkSpell Neutral TheCoin 0 $ \this ->
    OwnerOf this $ \you ->
        Effect $ GainManaCrystals you 1 CrystalTemporary


voidwalker :: Minion
voidwalker = mkMinion Warlock Voidwalker [Demon] 1 1 3 [
    Taunt ]


voodooDoctor :: Minion
voodooDoctor = mkMinion Neutral VoodooDoctor [] 1 2 1 [
    Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \character ->
            Effect $ RestoreHealth character 2 ]


waterElemental :: Minion
waterElemental = mkMinion Mage WaterElemental [] 4 3 6 [
    Whenever $ \this ->
        DamageIsDealt $ \victim _ source ->
            Effect $ when (this `Satisfies` [IsDamageSource source]) $ Freeze victim ]


warGolem :: Minion
warGolem = mkMinion Neutral WarGolem [] 7 7 7 []


whirlwind :: Spell
whirlwind = mkSpell Warrior Whirlwind 1 $ \this ->
    All $ Minions [] $ \minions ->
        Effect $ ForEach minions $ \minion ->
            (this `damages` minion) 1


-- TODO: ExcessMana effect
wildGrowth :: Spell
wildGrowth = mkSpell Druid WildGrowth 2 $ \this ->
    OwnerOf this $ \you ->
        Effect $ If (you `Satisfies` [HasMaxManaCrystals])
            (PutInHand you $ SpellCard excessMana)
            $ GainManaCrystals you 1 CrystalEmpty


wolfRider :: Minion
wolfRider = mkMinion Neutral WolfRider [] 3 3 1 [
    Charge ]








