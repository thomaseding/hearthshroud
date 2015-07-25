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
    bluegillWarrior,
    bloodfenRaptor,
    boulderfistOgre,
    chillwindYeti,
    coreHound,
    dreadInfernal,
    elvenArcher,
    fireElemental,
    frostwolfGrunt,
    innervate,
    ironforgeRifleman,
    magmaRager,
    moonfire,
    murlocRaider,
    noviceEngineer,
    oasisSnapjaw,
    recklessRocketeer,
    riverCrocolisk,
    shatteredSunCleric,
    starfire,
    stonetuskBoar,
    stormpikeCommando,
    stormwindKnight,
    swipe,
    theCoin,
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


bluegillWarrior :: DeckCard
bluegillWarrior = minion BluegillWarrior 2 2 1 [
    KeywordAbility Charge ]


bloodfenRaptor :: DeckCard
bloodfenRaptor = minion BloodfenRaptor 2 3 2 []


boulderfistOgre :: DeckCard
boulderfistOgre = minion BoulderfistOgre 6 6 7 []


chillwindYeti :: DeckCard
chillwindYeti = minion ChillwindYeti 4 4 5 []


coreHound :: DeckCard
coreHound = minion CoreHound 7 9 5 []


dreadInfernal :: DeckCard
dreadInfernal = minion DreadInfernal 6 6 6 [
    KeywordAbility $ Battlecry $ \this ->
        Elect $ OtherCharacters this $ \victim ->
            DealDamage victim 1 ]


elvenArcher :: DeckCard
elvenArcher = minion ElvenArcher 1 1 1 [
    KeywordAbility $ Battlecry $ \this ->
        Elect $ AnotherCharacter Targeted this $ \target ->
            DealDamage target 1 ]


fireElemental :: DeckCard
fireElemental = minion FireElemental 6 6 5 [
    KeywordAbility $ Battlecry $ \this ->
        Elect $ AnotherCharacter Targeted this $ \target ->
            DealDamage target 3 ]


frostwolfGrunt :: DeckCard
frostwolfGrunt = minion FrostwolfGrunt 2 2 2 [
    KeywordAbility Taunt ]


innervate :: DeckCard
innervate = spell Innervate 0 $ \this ->
    Elect $ CasterOf this $ \caster ->
        Sequence $ replicate 2 $ GainManaCrystal caster CrystalTemporary


ironforgeRifleman :: DeckCard
ironforgeRifleman = minion IronforgeRifleman 3 2 2 [
    KeywordAbility $ Battlecry $ \this ->
        Elect $ AnotherCharacter Targeted this $ \target ->
            DealDamage target 1 ]


magmaRager :: DeckCard
magmaRager = minion MagmaRager 3 5 1 []


moonfire :: DeckCard
moonfire = spell Moonfire 0 $ \_ ->
    Elect $ AnyCharacter Targeted $ \target ->
        DealDamage target 1


murlocRaider :: DeckCard
murlocRaider = minion MurlocRaider 1 2 1 []


noviceEngineer :: DeckCard
noviceEngineer = minion NoviceEngineer 2 1 1 [
    KeywordAbility $ Battlecry $ \this ->
        Elect $ ControllerOf this $ \controller ->
            DrawCards controller 1 ]


oasisSnapjaw :: DeckCard
oasisSnapjaw = minion OasisSnapjaw 4 2 7 []


recklessRocketeer :: DeckCard
recklessRocketeer = minion RecklessRocketeer 6 5 2 [
    KeywordAbility Charge ]


riverCrocolisk :: DeckCard
riverCrocolisk = minion RiverCrocolisk 2 2 3 []


shatteredSunCleric :: DeckCard
shatteredSunCleric = minion ShatteredSunCleric 3 3 2 [
    KeywordAbility $ Battlecry $ \this ->
        Elect $ AnotherFriendlyMinion Targeted this $ \target ->
            Enchant target [StatsDelta 1 1]]


starfire :: DeckCard
starfire = spell Starfire 6 $ \this ->
    Sequence [
        Elect $ AnyCharacter Targeted $ \target ->
            DealDamage target 5,
        Elect $ CasterOf this $ \caster ->
            DrawCards caster 1 ]


stonetuskBoar :: DeckCard
stonetuskBoar = minion StonetuskBoar 1 1 1 [
    KeywordAbility Charge ]


stormpikeCommando :: DeckCard
stormpikeCommando = minion StormpikeCommando 5 4 2 [
    KeywordAbility $ Battlecry $ \this ->
        Elect $ AnotherCharacter Targeted this $ \target ->
            DealDamage target 2 ]


stormwindKnight :: DeckCard
stormwindKnight = minion StormwindKnight 4 2 5 [
    KeywordAbility Charge ]


swipe :: DeckCard
swipe = spell Swipe 4 $ \_ ->
    Elect $ AnyEnemy Targeted $ \target ->
        Sequence [
            DealDamage target 4,
            Elect $ OtherEnemies target $ \other ->
                DealDamage other 1 ]


theCoin :: DeckCard
theCoin = spell TheCoin 0 $ \this ->
    Elect $ CasterOf this $ \caster ->
        GainManaCrystal caster CrystalTemporary


warGolem :: DeckCard
warGolem = minion WarGolem 7 7 7 []


wildGrowth :: DeckCard
wildGrowth = spell WildGrowth 2 $ \this ->
    Elect $ CasterOf this $ \caster ->
        GainManaCrystal caster CrystalEmpty


wolfRider :: DeckCard
wolfRider = minion WolfRider 3 3 1 [
    KeywordAbility Charge ]








