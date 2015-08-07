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
    assassinate,
    bluegillWarrior,
    bloodfenRaptor,
    bootyBayBodyguard,
    boulderfistOgre,
    chillwindYeti,
    coreHound,
    darkscaleHealer,
    dreadInfernal,
    elvenArcher,
    fireElemental,
    frostwolfGrunt,
    gnomishInventor,
    goldshireFootman,
    innervate,
    ironforgeRifleman,
    lordOfTheArena,
    magmaRager,
    moonfire,
    murlocRaider,
    nightblade,
    noviceEngineer,
    oasisSnapjaw,
    recklessRocketeer,
    riverCrocolisk,
    sen'jinShieldmasta,
    silverbackPatriarch,
    shatteredSunCleric,
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


minion :: BasicCardName -> Mana -> Attack -> Health -> [Ability] -> DeckCard
minion name mana attack health abilities = DeckCardMinion $ Minion {
    _minionCost = ManaCost mana,
    _minionAttack = attack,
    _minionHealth = health,
    _minionAbilities = abilities,
    _minionName = BasicCardName name }


spell :: BasicCardName -> Mana -> SpellEffect -> DeckCard
spell name mana effect = DeckCardSpell $ Spell {
    _spellCost = ManaCost mana,
    _spellEffect = effect,
    _spellName = BasicCardName name }


--------------------------------------------------------------------------------


assassinate :: DeckCard
assassinate = spell Assassinate 0 $ \_ ->
    Targeted $ AnyMinion $ \target ->
        Effect $ DestroyMinion target


bluegillWarrior :: DeckCard
bluegillWarrior = minion BluegillWarrior 2 2 1 [
    KeywordAbility Charge ]


bloodfenRaptor :: DeckCard
bloodfenRaptor = minion BloodfenRaptor 2 3 2 []


bootyBayBodyguard :: DeckCard
bootyBayBodyguard = minion BootyBayBodyguard 5 5 4 [
    KeywordAbility Taunt ]


boulderfistOgre :: DeckCard
boulderfistOgre = minion BoulderfistOgre 6 6 7 []


chillwindYeti :: DeckCard
chillwindYeti = minion ChillwindYeti 4 4 5 []


coreHound :: DeckCard
coreHound = minion CoreHound 7 9 5 []


darkscaleHealer :: DeckCard
darkscaleHealer = minion DarkscaleHealer 5 4 5 [
    KeywordAbility $ Battlecry $ \_ ->
        Effect $ With $ All $ FriendlyCharacters $ \friendlies ->
            ForEach friendlies $ \friendly ->
                RestoreHealth friendly 2 ]


dreadInfernal :: DeckCard
dreadInfernal = minion DreadInfernal 6 6 6 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ With $ All $ OtherCharacters (MinionCharacter this) $ \victims ->
            ForEach victims $ \victim ->
                DealDamage victim 1 ]


elvenArcher :: DeckCard
elvenArcher = minion ElvenArcher 1 1 1 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ AnotherCharacter (MinionCharacter this) $ \target ->
            Effect $ DealDamage target 1 ]


fireElemental :: DeckCard
fireElemental = minion FireElemental 6 6 5 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ AnotherCharacter (MinionCharacter this) $ \target ->
            Effect $ DealDamage target 3 ]


frostwolfGrunt :: DeckCard
frostwolfGrunt = minion FrostwolfGrunt 2 2 2 [
    KeywordAbility Taunt ]


gnomishInventor :: DeckCard
gnomishInventor = minion GnomishInventor 4 2 4 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ With $ Unique $ ControllerOf this $ \controller ->
            DrawCards controller 1 ]


goldshireFootman :: DeckCard
goldshireFootman = minion GoldshireFootman 1 1 2 [
    KeywordAbility Taunt ]


innervate :: DeckCard
innervate = spell Innervate 0 $ \this ->
    Effect $ With $ Unique $ CasterOf this $ \caster ->
        Sequence $ replicate 2 $ GainManaCrystal CrystalTemporary caster


ironforgeRifleman :: DeckCard
ironforgeRifleman = minion IronforgeRifleman 3 2 2 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ AnotherCharacter (MinionCharacter this) $ \target ->
            Effect $ DealDamage target 1 ]


lordOfTheArena :: DeckCard
lordOfTheArena = minion LordOfTheArena 6 6 5 [
    KeywordAbility Taunt ]


magmaRager :: DeckCard
magmaRager = minion MagmaRager 3 5 1 []


moonfire :: DeckCard
moonfire = spell Moonfire 0 $ \_ ->
    Targeted $ AnyCharacter $ \target ->
        Effect $ DealDamage target 1


murlocRaider :: DeckCard
murlocRaider = minion MurlocRaider 1 2 1 []


nightblade :: DeckCard
nightblade = minion Nightblade 5 4 4 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ With $ Unique $ ControllerOf this $ \controller ->
            With $ Unique $ OpponentOf controller $ \opponent ->
                DealDamage (PlayerCharacter opponent) 2 ]


noviceEngineer :: DeckCard
noviceEngineer = minion NoviceEngineer 2 1 1 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ With $ Unique $ ControllerOf this $ \controller ->
            DrawCards controller 1 ]


oasisSnapjaw :: DeckCard
oasisSnapjaw = minion OasisSnapjaw 4 2 7 []


recklessRocketeer :: DeckCard
recklessRocketeer = minion RecklessRocketeer 6 5 2 [
    KeywordAbility Charge ]


riverCrocolisk :: DeckCard
riverCrocolisk = minion RiverCrocolisk 2 2 3 []


sen'jinShieldmasta :: DeckCard
sen'jinShieldmasta = minion Sen'jinShieldmasta 4 3 5 [
    KeywordAbility Taunt ]


shatteredSunCleric :: DeckCard
shatteredSunCleric = minion ShatteredSunCleric 3 3 2 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ AnotherFriendlyMinion this $ \target ->
            Effect $ Enchant target [StatsDelta 1 1]]


silverbackPatriarch :: DeckCard
silverbackPatriarch = minion SilverbackPatriarch 3 1 4 [
    KeywordAbility Taunt ]


starfire :: DeckCard
starfire = spell Starfire 6 $ \this ->
    Targeted $ AnyCharacter $ \target ->
        Effect $ With $ Unique $ CasterOf this $ \caster ->
            Sequence [
                DealDamage target 5,
                DrawCards caster 1 ]


stonetuskBoar :: DeckCard
stonetuskBoar = minion StonetuskBoar 1 1 1 [
    KeywordAbility Charge ]


stormpikeCommando :: DeckCard
stormpikeCommando = minion StormpikeCommando 5 4 2 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ AnotherCharacter (MinionCharacter this) $ \target ->
            Effect $ DealDamage target 2 ]


stormwindKnight :: DeckCard
stormwindKnight = minion StormwindKnight 4 2 5 [
    KeywordAbility Charge ]


swipe :: DeckCard
swipe = spell Swipe 4 $ \_ ->
    Targeted $ AnyEnemy $ \target ->
        Effect $ With $ All $ OtherEnemies target $ \others ->
            Sequence [
                DealDamage target 4,
                ForEach others $ \other ->
                    DealDamage other 1 ]


theCoin :: DeckCard
theCoin = spell TheCoin 0 $ \this ->
    Effect $ With $ Unique $ CasterOf this $ \caster ->
        GainManaCrystal CrystalTemporary caster


voodooDoctor :: DeckCard
voodooDoctor = minion VoodooDoctor 1 2 1 [
    KeywordAbility $ Battlecry $ \_ ->
        Targeted $ AnyCharacter $ \character ->
            Effect $ RestoreHealth character 2 ]


warGolem :: DeckCard
warGolem = minion WarGolem 7 7 7 []


wildGrowth :: DeckCard
wildGrowth = spell WildGrowth 2 $ \this ->
    Effect $ With $ Unique $ CasterOf this $ \caster ->
        GainManaCrystal CrystalEmpty caster


wolfRider :: DeckCard
wolfRider = minion WolfRider 3 3 1 [
    KeywordAbility Charge ]








