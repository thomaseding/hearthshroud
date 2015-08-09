module Hearth.Cards.Basic (
    cards,
) where


--------------------------------------------------------------------------------


import Hearth.Model
import Hearth.Names
import Hearth.Names.Basic


--------------------------------------------------------------------------------


cards :: [DeckCard]
cards = [
    arcaneExplosion,
    arcaneIntellect,
    arcaneShot,
    assassinate,
    blessingOfKings,
    blessingOfMight,
    bluegillWarrior,
    bloodfenRaptor,
    bootyBayBodyguard,
    boulderfistOgre,
    chillwindYeti,
    consecration,
    coreHound,
    darkscaleHealer,
    drainLife,
    dreadInfernal,
    elvenArcher,
    fanOfKnives,
    fireball,
    fireElemental,
    flamestrike,
    frostwolfGrunt,
    gnomishInventor,
    goldshireFootman,
    guardianOfKings,
    hammerOfWrath,
    handOfProtection,
    healingTouch,
    hellfire,
    holyLight,
    holyNova,
    holySmite,
    ironbarkProtector,
    innervate,
    ironforgeRifleman,
    lordOfTheArena,
    magmaRager,
    markOfTheWild,
    mindBlast,
    moonfire,
    murlocRaider,
    nightblade,
    noviceEngineer,
    oasisSnapjaw,
    powerWordShield,
    recklessRocketeer,
    riverCrocolisk,
    sen'jinShieldmasta,
    shadowBolt,
    shatteredSunCleric,
    shiv,
    silverbackPatriarch,
    sinisterStrike,
    sprint,
    starfire,
    stonetuskBoar,
    stormpikeCommando,
    stormwindKnight,
    swipe,
    theCoin,
    voodooDoctor,
    warGolem,
    wildGrowth,
    wolfRider ]


--------------------------------------------------------------------------------


mkMinion :: BasicCardName -> Mana -> Attack -> Health -> [Ability] -> DeckCard
mkMinion name mana attack health abilities = DeckCardMinion $ Minion' {
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
    Effect $ With $ Unique (OwnerOf this) $ \owner ->
        With $ Unique (Not owner) $ \opponent ->
            With $ All $ Minions [OwnedBy opponent] $ \enemies ->
                ForEach enemies $ \enemy ->
                    DealDamage (MinionCharacter enemy) 1


arcaneIntellect :: DeckCard
arcaneIntellect = mkSpell ArcaneIntellect 3 $ \this ->
    Effect $ With $ Unique (OwnerOf this) $ \owner ->
        DrawCards owner 2


arcaneShot :: DeckCard
arcaneShot = mkSpell ArcaneShot 1 $ \_ ->
    Targeted $ Character [] $ \target ->
        Effect $ DealDamage target 2


assassinate :: DeckCard
assassinate = mkSpell Assassinate 0 $ \_ ->
    Targeted $ Minion [] $ \target ->
        Effect $ DestroyMinion target


blessingOfKings :: DeckCard
blessingOfKings = mkSpell BlessingOfKings 4 $ \_ ->
    Targeted $ Minion [] $ \target ->
        Effect $ Enchant target [
            StatsDelta 4 4 ]


blessingOfMight :: DeckCard
blessingOfMight = mkSpell BlessingOfMight 4 $ \_ ->
    Targeted $ Minion [] $ \target ->
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


chillwindYeti :: DeckCard
chillwindYeti = mkMinion ChillwindYeti 4 4 5 []


consecration :: DeckCard
consecration = mkSpell Consecration 4 $ \this ->
    Effect $ With $ Unique (OwnerOf this) $ \owner ->
        With $ Unique (Not owner) $ \opponent ->
            With $ All $ Characters [OwnedBy opponent] $ \enemies ->
                ForEach enemies $ \enemy ->
                    DealDamage enemy 2


coreHound :: DeckCard
coreHound = mkMinion CoreHound 7 9 5 []


darkscaleHealer :: DeckCard
darkscaleHealer = mkMinion DarkscaleHealer 5 4 5 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ With $ Unique (OwnerOf this) $ \owner ->
            With $ All $ Characters [OwnedBy owner] $ \friendlies ->
                ForEach friendlies $ \friendly ->
                    RestoreHealth friendly 2 ]


drainLife :: DeckCard
drainLife = mkSpell DrainLife 3 $ \this ->
    Targeted $ Character [] $ \target ->
        Effect $ With $ Unique (OwnerOf this) $ \owner ->
            Sequence [
                DealDamage target 2,
                RestoreHealth (PlayerCharacter owner) 2 ]


dreadInfernal :: DeckCard
dreadInfernal = mkMinion DreadInfernal 6 6 6 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ With $ All $ Characters [Not (MinionCharacter this)] $ \victims ->
            ForEach victims $ \victim ->
                DealDamage victim 1 ]


elvenArcher :: DeckCard
elvenArcher = mkMinion ElvenArcher 1 1 1 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ DealDamage target 1 ]


fanOfKnives :: DeckCard
fanOfKnives = mkSpell Consecration 4 $ \this ->
    Effect $ With $ Unique (OwnerOf this) $ \owner ->
        With $ Unique (Not owner) $ \opponent ->
            With $ All $ Minions [OwnedBy opponent] $ \enemies ->
                Sequence [
                    ForEach enemies $ \enemy ->
                        DealDamage (MinionCharacter enemy) 1,
                    DrawCards owner 1 ]


fireball :: DeckCard
fireball = mkSpell Fireball 4 $ \_ ->
    Targeted $ Character [] $ \target ->
        Effect $ DealDamage target 6


fireElemental :: DeckCard
fireElemental = mkMinion FireElemental 6 6 5 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ DealDamage target 3 ]


flamestrike :: DeckCard
flamestrike = mkSpell Flamestrike 7 $ \this ->
    Effect $ With $ Unique (OwnerOf this) $ \owner ->
        With $ Unique (Not owner) $ \opponent ->
            With $ All $ Minions [OwnedBy opponent] $ \victims ->
                ForEach victims $ \victim ->
                    DealDamage (MinionCharacter victim) 4


frostwolfGrunt :: DeckCard
frostwolfGrunt = mkMinion FrostwolfGrunt 2 2 2 [
    KeywordAbility Taunt ]


gnomishInventor :: DeckCard
gnomishInventor = mkMinion GnomishInventor 4 2 4 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ With $ Unique (OwnerOf this) $ \owner ->
            DrawCards owner 1 ]


goldshireFootman :: DeckCard
goldshireFootman = mkMinion GoldshireFootman 1 1 2 [
    KeywordAbility Taunt ]


guardianOfKings :: DeckCard
guardianOfKings = mkMinion GuardianOfKings 7 5 6 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ With $ Unique (OwnerOf this) $ \owner ->
            RestoreHealth (PlayerCharacter owner) 6 ]


hammerOfWrath :: DeckCard
hammerOfWrath = mkSpell HammerOfWrath 4 $ \this ->
    Targeted $ Character [] $ \target ->
        Effect $ With $ Unique (OwnerOf this) $ \owner ->
            Sequence [
                DealDamage target 3,
                DrawCards owner 1 ]


handOfProtection :: DeckCard
handOfProtection = mkSpell HandOfProtection 1 $ \_ ->
    Targeted $ Minion [] $ \target ->
        Effect $ GiveAbility target [
            KeywordAbility DivineShield ]


healingTouch :: DeckCard
healingTouch = mkSpell HealingTouch 3 $ \_ ->
    Targeted $ Character [] $ \target ->
        Effect $ RestoreHealth target 8


hellfire :: DeckCard
hellfire = mkSpell Hellfire 4 $ \_ ->
    Effect $ With $ All $ Characters [] $ \victims ->
        ForEach victims $ \victim ->
            DealDamage victim 3


holyLight :: DeckCard
holyLight = mkSpell HolyLight 2 $ \_ ->
    Targeted $ Character [] $ \target ->
        Effect $ RestoreHealth target 6


holyNova :: DeckCard
holyNova = mkSpell HolyNova 5 $ \this ->
    Effect $ With $ Unique (OwnerOf this) $ \owner ->
        With $ Unique (Not owner) $ \opponent ->
            With $ All $ Characters [OwnedBy owner] $ \friendlies ->
                With $ All $ Characters [OwnedBy opponent] $ \enemies ->
                    Sequence [
                        ForEach enemies $ \enemy ->
                            DealDamage enemy 2,
                        ForEach friendlies $ \friendly ->
                            RestoreHealth friendly 2 ]


holySmite :: DeckCard
holySmite = mkSpell HolySmite 1 $ \_ ->
    Targeted $ Character [] $ \target ->
        Effect $ DealDamage target 2


innervate :: DeckCard
innervate = mkSpell Innervate 0 $ \this ->
    Effect $ With $ Unique (OwnerOf this) $ \owner ->
        Sequence $ replicate 2 $ GainManaCrystal CrystalTemporary owner


ironbarkProtector :: DeckCard
ironbarkProtector = mkMinion IronbarkProtector 8 8 8 [
    KeywordAbility Taunt ]


ironforgeRifleman :: DeckCard
ironforgeRifleman = mkMinion IronforgeRifleman 3 2 2 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ DealDamage target 1 ]


lordOfTheArena :: DeckCard
lordOfTheArena = mkMinion LordOfTheArena 6 6 5 [
    KeywordAbility Taunt ]


markOfTheWild :: DeckCard
markOfTheWild = mkSpell MarkOfTheWild 2 $ \_ ->
    Targeted $ Minion [] $ \target ->
        Effect $ Sequence [
            GiveAbility target [
                KeywordAbility Taunt ],
            Enchant target [
                StatsDelta 2 2 ]]


magmaRager :: DeckCard
magmaRager = mkMinion MagmaRager 3 5 1 []


mindBlast :: DeckCard
mindBlast = mkSpell MindBlast 2 $ \this ->
    Effect $ With $ Unique (OwnerOf this) $ \owner ->
        With $ Unique (Not owner) $ \opponent ->
            DealDamage (PlayerCharacter opponent) 5


moonfire :: DeckCard
moonfire = mkSpell Moonfire 0 $ \_ ->
    Targeted $ Character [] $ \target ->
        Effect $ DealDamage target 1


murlocRaider :: DeckCard
murlocRaider = mkMinion MurlocRaider 1 2 1 []


nightblade :: DeckCard
nightblade = mkMinion Nightblade 5 4 4 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ With $ Unique (OwnerOf this) $ \owner ->
            With $ Unique (Not owner) $ \opponent ->
                DealDamage (PlayerCharacter opponent) 2 ]


noviceEngineer :: DeckCard
noviceEngineer = mkMinion NoviceEngineer 2 1 1 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ With $ Unique (OwnerOf this) $ \owner ->
            DrawCards owner 1 ]


oasisSnapjaw :: DeckCard
oasisSnapjaw = mkMinion OasisSnapjaw 4 2 7 []


powerWordShield :: DeckCard
powerWordShield = mkSpell PowerWordShield 1 $ \this ->
    Targeted $ Minion [] $ \target ->
        Effect $ With $ Unique (OwnerOf this) $ \owner ->
            Sequence [
                Enchant target [
                    StatsDelta 0 2 ],
                DrawCards owner 1 ]


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
    Targeted $ Minion [] $ \target ->
        Effect $ DealDamage (MinionCharacter target) 4


shatteredSunCleric :: DeckCard
shatteredSunCleric = mkMinion ShatteredSunCleric 3 3 2 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ Player (OwnerOf this) $ \owner ->
            Minion [OwnedBy owner, Not this] $ \target ->
                Effect $ Enchant target [
                    StatsDelta 1 1 ]]


shiv :: DeckCard
shiv = mkSpell Shiv 2 $ \this ->
    Targeted $ Character [] $ \target ->
        Effect $ With $ Unique (OwnerOf this) $ \owner ->
            Sequence [
                DealDamage target 1,
                DrawCards owner 1 ]


silverbackPatriarch :: DeckCard
silverbackPatriarch = mkMinion SilverbackPatriarch 3 1 4 [
    KeywordAbility Taunt ]


sinisterStrike :: DeckCard
sinisterStrike = mkSpell SinisterStrike 1 $ \this ->
    Effect $ With $ Unique (OwnerOf this) $ \owner ->
        With $ Unique (Not owner) $ \opponent ->
            DealDamage (PlayerCharacter opponent) 3


sprint :: DeckCard
sprint = mkSpell Sprint 7 $ \this ->
    Effect $ With $ Unique (OwnerOf this) $ \owner ->
        DrawCards owner 4


starfire :: DeckCard
starfire = mkSpell Starfire 6 $ \this ->
    Targeted $ Character [] $ \target ->
        Effect $ With $ Unique (OwnerOf this) $ \owner ->
            Sequence [
                DealDamage target 5,
                DrawCards owner 1 ]


stonetuskBoar :: DeckCard
stonetuskBoar = mkMinion StonetuskBoar 1 1 1 [
    KeywordAbility Charge ]


stormpikeCommando :: DeckCard
stormpikeCommando = mkMinion StormpikeCommando 5 4 2 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ Character [Not (MinionCharacter this)] $ \target ->
            Effect $ DealDamage target 2 ]


stormwindKnight :: DeckCard
stormwindKnight = mkMinion StormwindKnight 4 2 5 [
    KeywordAbility Charge ]


swipe :: DeckCard
swipe = mkSpell Swipe 4 $ \this ->
    Targeted $ Player (OwnerOf this) $ \owner ->
        Player (Not owner) $ \opponent ->
            Character [OwnedBy opponent] $ \target ->
                Effect $ With $ All $ Characters [Not target] $ \others ->
                    Sequence [
                        DealDamage target 4,
                        ForEach others $ \other ->
                            DealDamage other 1 ]


theCoin :: DeckCard
theCoin = mkSpell TheCoin 0 $ \this ->
    Effect $ With $ Unique (OwnerOf this) $ \owner ->
        GainManaCrystal CrystalTemporary owner


voodooDoctor :: DeckCard
voodooDoctor = mkMinion VoodooDoctor 1 2 1 [
    KeywordAbility $ Battlecry $ \_ ->
        Targeted $ Character [] $ \character ->
            Effect $ RestoreHealth character 2 ]


warGolem :: DeckCard
warGolem = mkMinion WarGolem 7 7 7 []


wildGrowth :: DeckCard
wildGrowth = mkSpell WildGrowth 2 $ \this ->
    Effect $ With $ Unique (OwnerOf this) $ \owner ->
        GainManaCrystal CrystalEmpty owner


wolfRider :: DeckCard
wolfRider = mkMinion WolfRider 3 3 1 [
    KeywordAbility Charge ]








