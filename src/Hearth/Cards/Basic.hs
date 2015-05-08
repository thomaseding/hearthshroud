module Hearth.Cards.Basic (
    cards,
    minions,
    spells,
) where


--------------------------------------------------------------------------------


import Hearth.Model
import Hearth.Names
import Hearth.Names.Basic


--------------------------------------------------------------------------------


cards :: [DeckCard]
cards = concat [
    map DeckCardMinion minions,
    map DeckCardSpell spells ]


minions :: [Minion]
minions = [
    bluegillWarrior,
    bloodfenRaptor,
    boulderfistOgre,
    chillwindYeti,
    coreHound,
    dreadInfernal,
    elvenArcher,
    fireElemental,
    frostwolfGrunt,
    ironforgeRifleman,
    magmaRager,
    murlocRaider,
    noviceEngineer,
    oasisSnapjaw,
    recklessRocketeer,
    riverCrocolisk,
    shatteredSunCleric,
    stonetuskBoar,
    stormpikeCommando,
    stormwindKnight,
    warGolem,
    wolfRider ]


spells :: [Spell]
spells = [
    innervate,
    moonfire,
    starfire,
    theCoin,
    wildGrowth ]


--------------------------------------------------------------------------------


minion :: BasicCardName -> Mana -> Attack -> Health -> [Ability] -> Minion
minion name mana attack health abilities = Minion {
    _minionCost = ManaCost mana,
    _minionAttack = attack,
    _minionHealth = health,
    _minionAbilities = abilities,
    _minionName = BasicCardName name }


spell :: BasicCardName -> Mana -> SpellEffect -> Spell
spell name mana effect = Spell {
    _spellCost = ManaCost mana,
    _spellEffect = effect,
    _spellName = BasicCardName name }


--------------------------------------------------------------------------------


bluegillWarrior :: Minion
bluegillWarrior = minion BluegillWarrior 2 2 1 [
    KeywordAbility Charge ]


bloodfenRaptor :: Minion
bloodfenRaptor = minion BloodfenRaptor 2 3 2 []


boulderfistOgre :: Minion
boulderfistOgre = minion BoulderfistOgre 6 6 7 []


chillwindYeti :: Minion
chillwindYeti = minion ChillwindYeti 4 4 5 []


coreHound :: Minion
coreHound = minion CoreHound 7 9 5 []


dreadInfernal :: Minion
dreadInfernal = minion DreadInfernal 6 6 6 [
    KeywordAbility $ Battlecry $ \this -> With $ OtherCharacters this $ \victim -> DealDamage victim 1 ]


elvenArcher :: Minion
elvenArcher = minion ElvenArcher 1 1 1 [
    KeywordAbility $ Battlecry $ \this -> With $ AnotherCharacter this $ \target -> DealDamage target 1 ]


fireElemental :: Minion
fireElemental = minion FireElemental 6 6 5 [
    KeywordAbility $ Battlecry $ \this -> With $ AnotherCharacter this $ \target -> DealDamage target 3 ]


frostwolfGrunt :: Minion
frostwolfGrunt = minion FrostwolfGrunt 2 2 2 [
    KeywordAbility Taunt ]


innervate :: Spell
innervate = spell Innervate 0 $ \this -> With $ CasterOf this $ \caster -> Sequence $ replicate 2 $ GainManaCrystal caster CrystalTemporary


ironforgeRifleman :: Minion
ironforgeRifleman = minion IronforgeRifleman 3 2 2 [
    KeywordAbility $ Battlecry $ \this -> With $ AnotherCharacter this $ \target -> DealDamage target 1 ]


magmaRager :: Minion
magmaRager = minion MagmaRager 3 5 1 []


moonfire :: Spell
moonfire = spell Moonfire 0 $ \_ -> With $ AnyCharacter $ \target -> DealDamage target 1


murlocRaider :: Minion
murlocRaider = minion MurlocRaider 1 2 1 []


noviceEngineer :: Minion
noviceEngineer = minion NoviceEngineer 2 1 1 [
    KeywordAbility $ Battlecry $ \this -> With $ ControllerOf this $ \controller -> DrawCards controller 1 ]


oasisSnapjaw :: Minion
oasisSnapjaw = minion OasisSnapjaw 4 2 7 []


recklessRocketeer :: Minion
recklessRocketeer = minion RecklessRocketeer 6 5 2 [
    KeywordAbility Charge ]


riverCrocolisk :: Minion
riverCrocolisk = minion RiverCrocolisk 2 2 3 []


shatteredSunCleric :: Minion
shatteredSunCleric = minion ShatteredSunCleric 3 3 2 [
    KeywordAbility $ Battlecry $ \this -> With $ AnotherFriendlyMinion this $ \target -> Enchant target [StatsDelta 1 1]]


starfire :: Spell
starfire = spell Starfire 6 $ \this -> Sequence [
    With $ AnyCharacter $ \target -> DealDamage target 5,
    With $ CasterOf this $ \caster -> DrawCards caster 1 ]


stonetuskBoar :: Minion
stonetuskBoar = minion StonetuskBoar 1 1 1 [
    KeywordAbility Charge ]


stormpikeCommando :: Minion
stormpikeCommando = minion StormpikeCommando 5 4 2 [
    KeywordAbility $ Battlecry $ \this -> With $ AnotherCharacter this $ \target -> DealDamage target 2 ]


stormwindKnight :: Minion
stormwindKnight = minion StormwindKnight 4 2 5 [
    KeywordAbility Charge ]


theCoin :: Spell
theCoin = spell TheCoin 0 $ \this -> With $ CasterOf this $ \caster -> GainManaCrystal caster CrystalTemporary


warGolem :: Minion
warGolem = minion WarGolem 7 7 7 []


wildGrowth :: Spell
wildGrowth = spell WildGrowth 2 $ \this -> With $ CasterOf this $ \caster -> GainManaCrystal caster CrystalEmpty


wolfRider :: Minion
wolfRider = minion WolfRider 3 3 1 [
    KeywordAbility Charge ]








