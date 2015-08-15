module Hearth.Cards.Basic (
    cards,
) where


--------------------------------------------------------------------------------


import Hearth.Model
import Hearth.Names
import Hearth.Names.Basic hiding (Charge)
import qualified Hearth.Names.Basic as Basic


--------------------------------------------------------------------------------


cards :: [DeckCard]
cards = [
    arcaneExplosion,
    arcaneIntellect,
    arcaneShot,
    assassinate,
    backstab,
    blessingOfKings,
    blessingOfMight,
    bluegillWarrior,
    bloodfenRaptor,
    bootyBayBodyguard,
    boulderfistOgre,
    charge,
    chillwindYeti,
    cleave,
    consecration,
    coreHound,
    darkscaleHealer,
    deadlyShot,
    divineSpirit,
    drainLife,
    dreadInfernal,
    elvenArcher,
    execute,
    fanOfKnives,
    fireball,
    fireElemental,
    flamestrike,
    frog,
    frostwolfGrunt,
    gnomishInventor,
    goldshireFootman,
    guardianOfKings,
    hammerOfWrath,
    handOfProtection,
    healingTouch,
    hellfire,
    hex,
    holyLight,
    holyNova,
    holySmite,
    humility,
    hunter'sMark,
    ironbarkProtector,
    innervate,
    ironforgeRifleman,
    kor'kronElite,
    lordOfTheArena,
    magmaRager,
    markOfTheWild,
    mindBlast,
    moonfire,
    multiShot,
    murlocRaider,
    nightblade,
    noviceEngineer,
    oasisSnapjaw,
    polymorph,
    powerWordShield,
    recklessRocketeer,
    riverCrocolisk,
    sen'jinShieldmasta,
    shadowBolt,
    shadowWordDeath,
    shadowWordPain,
    shatteredSunCleric,
    sheep,
    shiv,
    silverbackPatriarch,
    sinisterStrike,
    sprint,
    starfire,
    stonetuskBoar,
    stormpikeCommando,
    stormwindChampion,
    stormwindKnight,
    swipe,
    theCoin,
    voidwalker,
    voodooDoctor,
    warGolem,
    whirlwind,
    wildGrowth,
    wolfRider ]


--------------------------------------------------------------------------------


mkMinion :: BasicCardName -> Mana -> Attack -> Health -> [Ability] -> DeckCard
mkMinion name mana attack health abilities = DeckCardMinion $ mkMinion' name mana attack health abilities


mkMinion' :: BasicCardName -> Mana -> Attack -> Health -> [Ability] -> Minion
mkMinion' name mana attack health abilities = Minion' {
    _minionCost = ManaCost mana,
    _minionAttack = attack,
    _minionHealth = health,
    _minionAbilities = abilities,
    _minionName = BasicCardName name }


mkSpell :: BasicCardName -> Mana -> SpellEffect -> DeckCard
mkSpell name mana effect = DeckCardSpell $ Spell' {
    _spellCost = ManaCost mana,
    _spellEffect = effect,
    _spellName = BasicCardName name }


--------------------------------------------------------------------------------


arcaneExplosion :: DeckCard
arcaneExplosion = mkSpell ArcaneExplosion 2 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \enemies ->
                Effect $ ForEach enemies $ \enemy ->
                    DealDamage (MinionCharacter enemy) 1


arcaneIntellect :: DeckCard
arcaneIntellect = mkSpell ArcaneIntellect 3 $ \this ->
    OwnerOf this $ \you ->
        Effect $ DrawCards you 2


arcaneShot :: DeckCard
arcaneShot = mkSpell ArcaneShot 1 $ \_ ->
    A $ Character [] $ \target ->
        Effect $ DealDamage target 2


assassinate :: DeckCard
assassinate = mkSpell Assassinate 5 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ DestroyMinion target


backstab :: DeckCard
backstab = mkSpell Backstab 0 $ \_ ->
    A $ Minion [WithMinion Undamaged] $ \target ->
        Effect $ DealDamage (MinionCharacter target) 2


blessingOfKings :: DeckCard
blessingOfKings = mkSpell BlessingOfKings 4 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target [
            StatsDelta 4 4 ]


blessingOfMight :: DeckCard
blessingOfMight = mkSpell BlessingOfMight 4 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target [
            StatsDelta 3 0 ]


bluegillWarrior :: DeckCard
bluegillWarrior = mkMinion BluegillWarrior 2 2 1 [
    KeywordAbility Charge ]


bloodfenRaptor :: DeckCard
bloodfenRaptor = mkMinion BloodfenRaptor 2 3 2 []


bootyBayBodyguard :: DeckCard
bootyBayBodyguard = mkMinion BootyBayBodyguard 5 5 4 [
    KeywordAbility Taunt ]


boulderfistOgre :: DeckCard
boulderfistOgre = mkMinion BoulderfistOgre 6 6 7 []


charge :: DeckCard
charge = mkSpell Basic.Charge 3 $ \this ->
    OwnerOf this $ \you ->
        A $ Minion [OwnedBy you] $ \target ->
            Effect $ Sequence [
                Enchant target [
                    StatsDelta 2 0 ],
                GrantAbilities target [
                    KeywordAbility Charge ]]


chillwindYeti :: DeckCard
chillwindYeti = mkMinion ChillwindYeti 4 4 5 []


cleave :: DeckCard
cleave = mkSpell Cleave 2 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ Elect $ A $ Minion [OwnedBy opponent] $ \victim1 ->
                A $ Minion [OwnedBy opponent, Not victim1] $ \victim2 ->
                    Effect $ ForEach [victim1, victim2] $ \victim ->
                        DealDamage (MinionCharacter victim) 2


consecration :: DeckCard
consecration = mkSpell Consecration 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Characters [OwnedBy opponent] $ \enemies ->
                Effect $ ForEach enemies $ \enemy ->
                    DealDamage enemy 2


coreHound :: DeckCard
coreHound = mkMinion CoreHound 7 9 5 []


darkscaleHealer :: DeckCard
darkscaleHealer = mkMinion DarkscaleHealer 5 4 5 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            All $ Characters [OwnedBy you] $ \friendlies ->
                Effect $ ForEach friendlies $ \friendly ->
                    RestoreHealth friendly 2 ]


deadlyShot :: DeckCard
deadlyShot = mkSpell DeadlyShot 3 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ Elect $ A $ Minion [OwnedBy opponent] $ \victim ->
                Effect $ DestroyMinion victim


divineSpirit :: DeckCard
divineSpirit = mkSpell DivineSpirit 2 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target [
            StatsScale 1 2 ]


drainLife :: DeckCard
drainLife = mkSpell DrainLife 3 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                DealDamage target 2,
                RestoreHealth (PlayerCharacter you) 2 ]


dreadInfernal :: DeckCard
dreadInfernal = mkMinion DreadInfernal 6 6 6 [
    KeywordAbility $ Battlecry $ \this ->
        All $ Characters [Not (MinionCharacter this)] $ \victims ->
            Effect $ ForEach victims $ \victim ->
                DealDamage victim 1 ]


elvenArcher :: DeckCard
elvenArcher = mkMinion ElvenArcher 1 1 1 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ DealDamage target 1 ]


execute :: DeckCard
execute = mkSpell Execute 1 $ \_ ->
    A $ Minion [WithMinion Damaged] $ \target ->
        Effect $ DestroyMinion target


fanOfKnives :: DeckCard
fanOfKnives = mkSpell FanOfKnives 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \enemies ->
                Effect $ Sequence [
                    ForEach enemies $ \enemy ->
                        DealDamage (MinionCharacter enemy) 1,
                    DrawCards you 1 ]


fireball :: DeckCard
fireball = mkSpell Fireball 4 $ \_ ->
    A $ Character [] $ \target ->
        Effect $ DealDamage target 6


fireElemental :: DeckCard
fireElemental = mkMinion FireElemental 6 6 5 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ DealDamage target 3 ]


flamestrike :: DeckCard
flamestrike = mkSpell Flamestrike 7 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ ForEach victims $ \victim ->
                    DealDamage (MinionCharacter victim) 4


frog :: DeckCard
frog = DeckCardMinion frog'


frog' :: Minion
frog' = mkMinion' Frog 0 0 1 [
    KeywordAbility Taunt ]


frostwolfGrunt :: DeckCard
frostwolfGrunt = mkMinion FrostwolfGrunt 2 2 2 [
    KeywordAbility Taunt ]


gnomishInventor :: DeckCard
gnomishInventor = mkMinion GnomishInventor 4 2 4 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


goldshireFootman :: DeckCard
goldshireFootman = mkMinion GoldshireFootman 1 1 2 [
    KeywordAbility Taunt ]


guardianOfKings :: DeckCard
guardianOfKings = mkMinion GuardianOfKings 7 5 6 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ RestoreHealth (PlayerCharacter you) 6 ]


hammerOfWrath :: DeckCard
hammerOfWrath = mkSpell HammerOfWrath 4 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                DealDamage target 3,
                DrawCards you 1 ]


handOfProtection :: DeckCard
handOfProtection = mkSpell HandOfProtection 1 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ GrantAbilities target [
            KeywordAbility DivineShield ]


healingTouch :: DeckCard
healingTouch = mkSpell HealingTouch 3 $ \_ ->
    A $ Character [] $ \target ->
        Effect $ RestoreHealth target 8


hellfire :: DeckCard
hellfire = mkSpell Hellfire 4 $ \_ ->
    All $ Characters [] $ \victims ->
        Effect $ ForEach victims $ \victim ->
            DealDamage victim 3


hex :: DeckCard
hex = mkSpell Hex 3 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Transform target frog'


holyLight :: DeckCard
holyLight = mkSpell HolyLight 2 $ \_ ->
    A $ Character [] $ \target ->
        Effect $ RestoreHealth target 6


holyNova :: DeckCard
holyNova = mkSpell HolyNova 5 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Characters [OwnedBy you] $ \friendlies ->
                All $ Characters [OwnedBy opponent] $ \enemies ->
                    Effect $ Sequence [
                        ForEach enemies $ \enemy ->
                            DealDamage enemy 2,
                        ForEach friendlies $ \friendly ->
                            RestoreHealth friendly 2 ]


holySmite :: DeckCard
holySmite = mkSpell HolySmite 1 $ \_ ->
    A $ Character [] $ \target ->
        Effect $ DealDamage target 2


humility :: DeckCard
humility = mkSpell Humility 1 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target [
            ChangeStat (Left 1) ]


hunter'sMark :: DeckCard
hunter'sMark = mkSpell Hunter'sMark 0 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target [
            ChangeStat (Right 1) ]


kor'kronElite :: DeckCard
kor'kronElite = mkMinion Kor'kronElite 4 4 3 [
    KeywordAbility Charge ]


innervate :: DeckCard
innervate = mkSpell Innervate 0 $ \this ->
    OwnerOf this $ \you ->
        Effect $ GainManaCrystals you 2 CrystalTemporary


ironbarkProtector :: DeckCard
ironbarkProtector = mkMinion IronbarkProtector 8 8 8 [
    KeywordAbility Taunt ]


ironforgeRifleman :: DeckCard
ironforgeRifleman = mkMinion IronforgeRifleman 3 2 2 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ DealDamage target 1 ]


lordOfTheArena :: DeckCard
lordOfTheArena = mkMinion LordOfTheArena 6 6 5 [
    KeywordAbility Taunt ]


markOfTheWild :: DeckCard
markOfTheWild = mkSpell MarkOfTheWild 2 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Sequence [
            GrantAbilities target [
                KeywordAbility Taunt ],
            Enchant target [
                StatsDelta 2 2 ]]


magmaRager :: DeckCard
magmaRager = mkMinion MagmaRager 3 5 1 []


mindBlast :: DeckCard
mindBlast = mkSpell MindBlast 2 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ DealDamage (PlayerCharacter opponent) 5


moonfire :: DeckCard
moonfire = mkSpell Moonfire 0 $ \_ ->
    A $ Character [] $ \target ->
        Effect $ DealDamage target 1


multiShot :: DeckCard
multiShot = mkSpell MultiShot 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ Elect $ A $ Minion [OwnedBy opponent] $ \victim1 ->
                A $ Minion [OwnedBy opponent, Not victim1] $ \victim2 ->
                    Effect $ ForEach [victim1, victim2] $ \victim ->
                        DealDamage (MinionCharacter victim) 3


murlocRaider :: DeckCard
murlocRaider = mkMinion MurlocRaider 1 2 1 []


nightblade :: DeckCard
nightblade = mkMinion Nightblade 5 4 4 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ DealDamage (PlayerCharacter opponent) 3 ]


noviceEngineer :: DeckCard
noviceEngineer = mkMinion NoviceEngineer 2 1 1 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


oasisSnapjaw :: DeckCard
oasisSnapjaw = mkMinion OasisSnapjaw 4 2 7 []


polymorph :: DeckCard
polymorph = mkSpell Polymorph 4 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Transform target sheep'


powerWordShield :: DeckCard
powerWordShield = mkSpell PowerWordShield 1 $ \this ->
    A $ Minion [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                Enchant target [
                    StatsDelta 0 2 ],
                DrawCards you 1 ]


recklessRocketeer :: DeckCard
recklessRocketeer = mkMinion RecklessRocketeer 6 5 2 [
    KeywordAbility Charge ]


riverCrocolisk :: DeckCard
riverCrocolisk = mkMinion RiverCrocolisk 2 2 3 []


sen'jinShieldmasta :: DeckCard
sen'jinShieldmasta = mkMinion Sen'jinShieldmasta 4 3 5 [
    KeywordAbility Taunt ]


shadowBolt :: DeckCard
shadowBolt = mkSpell ShadowBolt 3 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ DealDamage (MinionCharacter target) 4


shadowWordDeath :: DeckCard
shadowWordDeath = mkSpell ShadowWordDeath 5 $ \_ ->
    A $ Minion [AttackCond GreaterEqual 5] $ \target ->
        Effect $ DestroyMinion target


shadowWordPain :: DeckCard
shadowWordPain = mkSpell ShadowWordPain 2 $ \_ ->
    A $ Minion [AttackCond LessEqual 3] $ \target ->
        Effect $ DestroyMinion target


shatteredSunCleric :: DeckCard
shatteredSunCleric = mkMinion ShatteredSunCleric 3 3 2 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion [OwnedBy you, Not this] $ \target ->
                Effect $ Enchant target [
                    StatsDelta 1 1 ]]


sheep :: DeckCard
sheep = DeckCardMinion sheep'


sheep' :: Minion
sheep' = mkMinion' Sheep 0 1 1 []


shiv :: DeckCard
shiv = mkSpell Shiv 2 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                DealDamage target 1,
                DrawCards you 1 ]


silverbackPatriarch :: DeckCard
silverbackPatriarch = mkMinion SilverbackPatriarch 3 1 4 [
    KeywordAbility Taunt ]


sinisterStrike :: DeckCard
sinisterStrike = mkSpell SinisterStrike 1 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            Effect $ DealDamage (PlayerCharacter opponent) 3


sprint :: DeckCard
sprint = mkSpell Sprint 7 $ \this ->
    OwnerOf this $ \you ->
        Effect $ DrawCards you 4


starfire :: DeckCard
starfire = mkSpell Starfire 6 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                DealDamage target 5,
                DrawCards you 1 ]


stonetuskBoar :: DeckCard
stonetuskBoar = mkMinion StonetuskBoar 1 1 1 [
    KeywordAbility Charge ]


stormpikeCommando :: DeckCard
stormpikeCommando = mkMinion StormpikeCommando 5 4 2 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ DealDamage target 2 ]


stormwindKnight :: DeckCard
stormwindKnight = mkMinion StormwindKnight 4 2 5 [
    KeywordAbility Charge ]


stormwindChampion :: DeckCard
stormwindChampion = mkMinion StormwindChampion 7 6 6 [
    Aura $ \this ->
        AuraOwnerOf this $ \you ->
            EachMinion [OwnedBy you, Not this] $ \minion ->
                Has minion [
                    StatsDelta 1 1 ]]


swipe :: DeckCard
swipe = mkSpell Swipe 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            A $ Character [OwnedBy opponent] $ \target ->
                All $ Characters [Not target] $ \others ->
                    Effect $ Sequence [
                        DealDamage target 4,
                        ForEach others $ \other ->
                            DealDamage other 1 ]


theCoin :: DeckCard
theCoin = mkSpell TheCoin 0 $ \this ->
    OwnerOf this $ \you ->
        Effect $ GainManaCrystals you 1 CrystalTemporary


voidwalker :: DeckCard
voidwalker = mkMinion Voidwalker 1 1 3 [
    KeywordAbility Taunt ]


voodooDoctor :: DeckCard
voodooDoctor = mkMinion VoodooDoctor 1 2 1 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \character ->
            Effect $ RestoreHealth character 2 ]


warGolem :: DeckCard
warGolem = mkMinion WarGolem 7 7 7 []


whirlwind :: DeckCard
whirlwind = mkSpell Whirlwind 1 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ ForEach minions $ \minion ->
            DealDamage (MinionCharacter minion) 1


wildGrowth :: DeckCard
wildGrowth = mkSpell WildGrowth 2 $ \this ->
    OwnerOf this $ \you ->
        Effect $ GainManaCrystals you 1 CrystalEmpty


wolfRider :: DeckCard
wolfRider = mkMinion WolfRider 3 3 1 [
    KeywordAbility Charge ]








