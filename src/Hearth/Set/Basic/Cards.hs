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


cards :: [DeckCard]
cards = let x = toCard in [
    x arcaneExplosion,
    x arcaneIntellect,
    x arcaneShot,
    x assassinate,
    x backstab,
    x blessingOfKings,
    x blessingOfMight,
    x bluegillWarrior,
    x bloodfenRaptor,
    x bootyBayBodyguard,
    x boulderfistOgre,
    x charge,
    x chillwindYeti,
    x cleave,
    x consecration,
    x coreHound,
    x darkscaleHealer,
    x deadlyShot,
    x divineSpirit,
    x drainLife,
    x dreadInfernal,
    x elvenArcher,
    x execute,
    x fanOfKnives,
    x fireball,
    x fireElemental,
    x flamestrike,
    x flametongueTotem,
    x frog,
    x frostbolt,
    x frostNova,
    x frostwolfGrunt,
    x gnomishInventor,
    x goldshireFootman,
    x guardianOfKings,
    x hammerOfWrath,
    x handOfProtection,
    x healingTouch,
    x hellfire,
    x hex,
    x holyLight,
    x holyNova,
    x holySmite,
    x humility,
    x hunter'sMark,
    x ironbarkProtector,
    x innervate,
    x ironforgeRifleman,
    x kor'kronElite,
    x lordOfTheArena,
    x magmaRager,
    x markOfTheWild,
    x mindBlast,
    x moonfire,
    x multiShot,
    x murlocRaider,
    x nightblade,
    x noviceEngineer,
    x oasisSnapjaw,
    x polymorph,
    x powerWordShield,
    x raidLeader,
    x recklessRocketeer,
    x riverCrocolisk,
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


mkMinion :: Class -> BasicCardName -> Mana -> Attack -> Health -> [Ability] -> Minion
mkMinion = mkMinion' BasicCardName Free


mkSpell :: Class -> BasicCardName -> Mana -> SpellEffect -> Spell
mkSpell = mkSpell' BasicCardName Free


--------------------------------------------------------------------------------


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
    A $ Minion [WithMinion Undamaged] $ \target ->
        Effect $ (this `damages` target) 2


blessingOfKings :: Spell
blessingOfKings = mkSpell Paladin BlessingOfKings 4 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ StatsDelta 4 4


blessingOfMight :: Spell
blessingOfMight = mkSpell Paladin BlessingOfMight 4 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ StatsDelta 3 0


bluegillWarrior :: Minion
bluegillWarrior = mkMinion Neutral BluegillWarrior 2 2 1 [
    KeywordAbility Charge ]


bloodfenRaptor :: Minion
bloodfenRaptor = mkMinion Neutral BloodfenRaptor 2 3 2 []


bootyBayBodyguard :: Minion
bootyBayBodyguard = mkMinion Neutral BootyBayBodyguard 5 5 4 [
    KeywordAbility Taunt ]


boulderfistOgre :: Minion
boulderfistOgre = mkMinion Neutral BoulderfistOgre 6 6 7 []


charge :: Spell
charge = mkSpell Warrior Basic.Charge 3 $ \this ->
    OwnerOf this $ \you ->
        A $ Minion [OwnedBy you] $ \target ->
            Effect $ Sequence [
                Enchant target $ Continuous $ StatsDelta 2 0,
                GrantAbilities target [
                    KeywordAbility Charge ]]


chillwindYeti :: Minion
chillwindYeti = mkMinion Neutral ChillwindYeti 4 4 5 []


cleave :: Spell
cleave = mkSpell Warrior Cleave 2 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ Elect $ A $ Minion [OwnedBy opponent] $ \victim1 ->
                A $ Minion [OwnedBy opponent, Not victim1] $ \victim2 ->
                    Effect $ ForEach (HandleList [victim1, victim2]) $ \victim ->
                        (this `damages` victim) 2


consecration :: Spell
consecration = mkSpell Paladin Consecration 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Characters [OwnedBy opponent] $ \enemies ->
                Effect $ ForEach enemies $ \enemy ->
                    (this `damages` enemy) 2


coreHound :: Minion
coreHound = mkMinion Neutral CoreHound 7 9 5 []


darkscaleHealer :: Minion
darkscaleHealer = mkMinion Neutral DarkscaleHealer 5 4 5 [
    KeywordAbility $ Battlecry $ \this ->
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


drainLife :: Spell
drainLife = mkSpell Warlock DrainLife 3 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 2,
                RestoreHealth (PlayerCharacter you) 2 ]


dreadInfernal :: Minion
dreadInfernal = mkMinion Warlock DreadInfernal 6 6 6 [
    KeywordAbility $ Battlecry $ \this ->
        All $ Characters [Not (MinionCharacter this)] $ \victims ->
            Effect $ ForEach victims $ \victim ->
                (this `damages` victim) 1 ]


elvenArcher :: Minion
elvenArcher = mkMinion Neutral ElvenArcher 1 1 1 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ (this `damages` target) 1 ]


execute :: Spell
execute = mkSpell Warrior Execute 1 $ \_ ->
    A $ Minion [WithMinion Damaged] $ \target ->
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
fireElemental = mkMinion Shaman FireElemental 6 6 5 [
    KeywordAbility $ Battlecry $ \this ->
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
flametongueTotem = mkMinion Shaman FlametongueTotem 2 0 3 [
    Aura $ \this ->
        EachMinion [AdjacentTo this] $ \minion ->
            Has minion $ StatsDelta 2 0 ]


frog :: Minion
frog = uncollectible $ mkMinion Neutral Frog 0 0 1 [
    KeywordAbility Taunt ]


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


frostwolfGrunt :: Minion
frostwolfGrunt = mkMinion Neutral FrostwolfGrunt 2 2 2 [
    KeywordAbility Taunt ]


gnomishInventor :: Minion
gnomishInventor = mkMinion Neutral GnomishInventor 4 2 4 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


goldshireFootman :: Minion
goldshireFootman = mkMinion Neutral GoldshireFootman 1 1 2 [
    KeywordAbility Taunt ]


guardianOfKings :: Minion
guardianOfKings = mkMinion Paladin GuardianOfKings 7 5 6 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ RestoreHealth (PlayerCharacter you) 6 ]


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
            KeywordAbility DivineShield ]


healingTouch :: Spell
healingTouch = mkSpell Druid HealingTouch 3 $ \_ ->
    A $ Character [] $ \target ->
        Effect $ RestoreHealth target 8


hellfire :: Spell
hellfire = mkSpell Warlock Hellfire 4 $ \this ->
    All $ Characters [] $ \victims ->
        Effect $ ForEach victims $ \victim ->
            (this `damages` victim) 3


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


humility :: Spell
humility = mkSpell Paladin Humility 1 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ ChangeStat (Left 1)


hunter'sMark :: Spell
hunter'sMark = mkSpell Hunter Hunter'sMark 0 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target $ Continuous $ ChangeStat (Right 1)


kor'kronElite :: Minion
kor'kronElite = mkMinion Warrior Kor'kronElite 4 4 3 [
    KeywordAbility Charge ]


innervate :: Spell
innervate = mkSpell Druid Innervate 0 $ \this ->
    OwnerOf this $ \you ->
        Effect $ GainManaCrystals you 2 CrystalTemporary


ironbarkProtector :: Minion
ironbarkProtector = mkMinion Druid IronbarkProtector 8 8 8 [
    KeywordAbility Taunt ]


ironforgeRifleman :: Minion
ironforgeRifleman = mkMinion Neutral IronforgeRifleman 3 2 2 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ (this `damages` target) 1 ]


lordOfTheArena :: Minion
lordOfTheArena = mkMinion Neutral LordOfTheArena 6 6 5 [
    KeywordAbility Taunt ]


markOfTheWild :: Spell
markOfTheWild = mkSpell Druid MarkOfTheWild 2 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Sequence [
            GrantAbilities target [
                KeywordAbility Taunt ],
            Enchant target $ Continuous $ StatsDelta 2 2 ]


magmaRager :: Minion
magmaRager = mkMinion Neutral MagmaRager 3 5 1 []


mindBlast :: Spell
mindBlast = mkSpell Priest MindBlast 2 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ (this `damages` opponent) 5


moonfire :: Spell
moonfire = mkSpell Druid Moonfire 0 $ \this ->
    A $ Character [] $ \target ->
        Effect $ (this `damages` target) 1


multiShot :: Spell
multiShot = mkSpell Hunter MultiShot 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ Elect $ A $ Minion [OwnedBy opponent] $ \victim1 ->
                A $ Minion [OwnedBy opponent, Not victim1] $ \victim2 ->
                    Effect $ ForEach (HandleList [victim1, victim2]) $ \victim ->
                        (this `damages` victim) 3


murlocRaider :: Minion
murlocRaider = mkMinion Neutral MurlocRaider 1 2 1 []


nightblade :: Minion
nightblade = mkMinion Neutral Nightblade 5 4 4 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ (this `damages` opponent) 3 ]


noviceEngineer :: Minion
noviceEngineer = mkMinion Neutral NoviceEngineer 2 1 1 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


oasisSnapjaw :: Minion
oasisSnapjaw = mkMinion Neutral OasisSnapjaw 4 2 7 []


polymorph :: Spell
polymorph = mkSpell Mage Polymorph 4 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Transform target sheep


powerWordShield :: Spell
powerWordShield = mkSpell Priest PowerWordShield 1 $ \this ->
    A $ Minion [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                Enchant target $ Continuous $ StatsDelta 0 2,
                DrawCards you 1 ]


raidLeader :: Minion
raidLeader = mkMinion Neutral RaidLeader 3 2 2 [
    Aura $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, Not this] $ \minion ->
                Has minion $ StatsDelta 1 0 ]


recklessRocketeer :: Minion
recklessRocketeer = mkMinion Neutral RecklessRocketeer 6 5 2 [
    KeywordAbility Charge ]


riverCrocolisk :: Minion
riverCrocolisk = mkMinion Neutral RiverCrocolisk 2 2 3 []


sen'jinShieldmasta :: Minion
sen'jinShieldmasta = mkMinion Neutral Sen'jinShieldmasta 4 3 5 [
    KeywordAbility Taunt ]


shadowBolt :: Spell
shadowBolt = mkSpell Warlock ShadowBolt 3 $ \this ->
    A $ Minion [] $ \target ->
        Effect $ (this `damages` target) 4


shadowWordDeath :: Spell
shadowWordDeath = mkSpell Priest ShadowWordDeath 5 $ \_ ->
    A $ Minion [AttackCond GreaterEqual 5] $ \target ->
        Effect $ DestroyMinion target


shadowWordPain :: Spell
shadowWordPain = mkSpell Priest ShadowWordPain 2 $ \_ ->
    A $ Minion [AttackCond LessEqual 3] $ \target ->
        Effect $ DestroyMinion target


shatteredSunCleric :: Minion
shatteredSunCleric = mkMinion Neutral ShatteredSunCleric 3 3 2 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion [OwnedBy you, Not this] $ \target ->
                Effect $ Enchant target $ Continuous $ StatsDelta 1 1 ]


sheep :: Minion
sheep = uncollectible $ mkMinion Neutral Sheep 0 1 1 []


shiv :: Spell
shiv = mkSpell Rogue Shiv 2 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                (this `damages` target) 1,
                DrawCards you 1 ]


silverbackPatriarch :: Minion
silverbackPatriarch = mkMinion Neutral SilverbackPatriarch 3 1 4 [
    KeywordAbility Taunt ]


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


stonetuskBoar :: Minion
stonetuskBoar = mkMinion Neutral StonetuskBoar 1 1 1 [
    KeywordAbility Charge ]


stormpikeCommando :: Minion
stormpikeCommando = mkMinion Neutral StormpikeCommando 5 4 2 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ (this `damages` target) 2 ]


stormwindKnight :: Minion
stormwindKnight = mkMinion Neutral StormwindKnight 4 2 5 [
    KeywordAbility Charge ]


stormwindChampion :: Minion
stormwindChampion = mkMinion Neutral StormwindChampion 7 6 6 [
    Aura $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, Not this] $ \minion ->
                Has minion $ StatsDelta 1 1 ]


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
voidwalker = mkMinion Warlock Voidwalker 1 1 3 [
    KeywordAbility Taunt ]


voodooDoctor :: Minion
voodooDoctor = mkMinion Neutral VoodooDoctor 1 2 1 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \character ->
            Effect $ RestoreHealth character 2 ]


waterElemental :: Minion
waterElemental = mkMinion Mage WaterElemental 4 3 6 [
    Whenever $ \this ->
        DamageIsDealt $ \victim _ source ->
            Effect $ when (MinionCharacter this `Satisfies` [IsDamageSource source]) $ Freeze victim ]


warGolem :: Minion
warGolem = mkMinion Neutral WarGolem 7 7 7 []


whirlwind :: Spell
whirlwind = mkSpell Warrior Whirlwind 1 $ \this ->
    All $ Minions [] $ \minions ->
        Effect $ ForEach minions $ \minion ->
            (this `damages` minion) 1


-- TODO: ExcessMana effect
wildGrowth :: Spell
wildGrowth = mkSpell Druid WildGrowth 2 $ \this ->
    OwnerOf this $ \you ->
        Effect $ GainManaCrystals you 1 CrystalEmpty


wolfRider :: Minion
wolfRider = mkMinion Neutral WolfRider 3 3 1 [
    KeywordAbility Charge ]








