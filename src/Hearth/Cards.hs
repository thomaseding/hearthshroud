module Hearth.Cards where


--------------------------------------------------------------------------------


import Hearth.Model
import Hearth.Names


--------------------------------------------------------------------------------


cardUniverse :: [DeckCard]
cardUniverse = map DeckCardMinion minionUniverse ++ map DeckCardSpell spellUniverse


minionUniverse :: [Minion]
minionUniverse = [
    amaniBerserker,
    argentCommander,
    argentProtector,
    argentSquire,
    bluegillWarrior,
    bloodfenRaptor,
    boulderfistOgre,
    chillwindYeti,
    coreHound,
    cruelTaskmaster,
    dreadInfernal,
    elvenArcher,
    fireElemental,
    frostwolfGrunt,
    ironbeakOwl,
    ironforgeRifleman,
    magmaRager,
    murlocRaider,
    noviceEngineer,
    oasisSnapjaw,
    recklessRocketeer,
    riverCrocolisk,
    scarletCrusader,
    silvermoonGuardian,
    shatteredSunCleric,
    spellbreaker,
    stonetuskBoar,
    stormpikeCommando,
    stormwindKnight,
    sunwalker,
    warGolem,
    wolfRider ]


spellUniverse :: [Spell]
spellUniverse = [
    moonfire,
    starfire ]


--------------------------------------------------------------------------------


mkMinion :: CardName -> Mana -> Attack -> Health -> [Ability] -> Minion
mkMinion name mana attack health abilities = Minion {
    _minionCost = ManaCost mana,
    _minionAttack = attack,
    _minionHealth = health,
    _minionAbilities = abilities,
    _minionName = name }


mkBasicMinion :: BasicCardName -> Mana -> Attack -> Health -> [Ability] -> Minion
mkBasicMinion name = mkMinion $ BasicCardName name


mkClassicMinion :: ClassicCardName -> Mana -> Attack -> Health -> [Ability] -> Minion
mkClassicMinion name = mkMinion $ ClassicCardName name



--------------------------------------------------------------------------------


mkSpell :: CardName -> Mana -> SpellEffect -> Spell
mkSpell name mana effect = Spell {
    _spellCost = ManaCost mana,
    _spellEffect = effect,
    _spellName = name }


mkBasicSpell :: BasicCardName -> Mana -> SpellEffect -> Spell
mkBasicSpell name = mkSpell $ BasicCardName name


mkClassicSpell :: ClassicCardName -> Mana -> SpellEffect -> Spell
mkClassicSpell name = mkSpell $ ClassicCardName name



--------------------------------------------------------------------------------


amaniBerserker :: Minion
amaniBerserker = mkClassicMinion AmaniBerserker 2 2 3 [
    KeywordAbility $ Enrage [StatsDelta 3 0] ]


argentCommander :: Minion
argentCommander = mkClassicMinion ArgentCommander 6 4 2 [
    KeywordAbility Charge,
    KeywordAbility DivineShield ]


argentProtector :: Minion
argentProtector = mkClassicMinion ArgentProtector 2 2 2 [
    KeywordAbility $ Battlecry $ With . AnotherMinion (Give [KeywordAbility DivineShield]) ]


argentSquire :: Minion
argentSquire = mkClassicMinion ArgentSquire 1 1 1 [
    KeywordAbility DivineShield ]


bluegillWarrior :: Minion
bluegillWarrior = mkBasicMinion BluegillWarrior 2 2 1 [
    KeywordAbility Charge ]


bloodfenRaptor :: Minion
bloodfenRaptor = mkBasicMinion BloodfenRaptor 2 3 2 []


boulderfistOgre :: Minion
boulderfistOgre = mkBasicMinion BoulderfistOgre 6 6 7 []


chillwindYeti :: Minion
chillwindYeti = mkBasicMinion MurlocRaider 1 2 1 []


coreHound :: Minion
coreHound = mkBasicMinion CoreHound 7 9 5 []


cruelTaskmaster :: Minion
cruelTaskmaster = mkClassicMinion CruelTaskmaster 2 2 2 [
    KeywordAbility $ Battlecry $ With . AnotherMinion (\target -> Sequence [
        DealDamage 1 target,
        Enchant [StatsDelta 2 0] target ]) ]


dreadInfernal :: Minion
dreadInfernal = mkBasicMinion DreadInfernal 6 6 6 [
    KeywordAbility $ Battlecry $ With . OtherCharacters (DealDamage 1) ]


elvenArcher :: Minion
elvenArcher = mkBasicMinion ElvenArcher 1 1 1 [
    KeywordAbility $ Battlecry $ With . AnotherCharacter (DealDamage 1) ]


fireElemental :: Minion
fireElemental = mkBasicMinion FireElemental 6 6 5 [
    KeywordAbility $ Battlecry $ With . AnotherCharacter (DealDamage 3) ]


frostwolfGrunt :: Minion
frostwolfGrunt = mkBasicMinion FrostwolfGrunt 2 2 2 [
    KeywordAbility Taunt ]


ironbeakOwl :: Minion
ironbeakOwl = mkClassicMinion IronbeakOwl 2 2 1 [
    KeywordAbility $ Battlecry $ With . AnotherMinion (KeywordEffect . Silence) ]


ironforgeRifleman :: Minion
ironforgeRifleman = mkBasicMinion IronforgeRifleman 3 2 2 [
    KeywordAbility $ Battlecry $ With . AnotherCharacter (DealDamage 1) ]


magmaRager :: Minion
magmaRager = mkBasicMinion MagmaRager 3 5 1 []


moonfire :: Spell
moonfire = mkBasicSpell Moonfire 0 $ const $ With $ AnyCharacter $ DealDamage 1


murlocRaider :: Minion
murlocRaider = mkBasicMinion MurlocRaider 1 2 1 []


noviceEngineer :: Minion
noviceEngineer = mkBasicMinion NoviceEngineer 2 1 1 [
    KeywordAbility $ Battlecry $ With . ControllerOf (DrawCards 1) ]


oasisSnapjaw :: Minion
oasisSnapjaw = mkBasicMinion OasisSnapjaw 4 2 7 []


recklessRocketeer :: Minion
recklessRocketeer = mkBasicMinion RecklessRocketeer 6 5 2 [
    KeywordAbility Charge ]


riverCrocolisk :: Minion
riverCrocolisk = mkBasicMinion RiverCrocolisk 2 2 3 []


scarletCrusader :: Minion
scarletCrusader = mkClassicMinion ScarletCrusader 3 3 1 [
    KeywordAbility DivineShield ]


silvermoonGuardian :: Minion
silvermoonGuardian = mkClassicMinion SilvermoonGuardian 4 3 3 [
    KeywordAbility DivineShield ]


shatteredSunCleric :: Minion
shatteredSunCleric = mkBasicMinion ShatteredSunCleric 3 3 2 [
    KeywordAbility $ Battlecry $ With . AnotherFriendlyMinion (Enchant [StatsDelta 1 1]) ]


spellbreaker :: Minion
spellbreaker = mkClassicMinion Spellbreaker 4 4 3 [
    KeywordAbility $ Battlecry $ With . AnotherFriendlyMinion (KeywordEffect . Silence) ]


starfire :: Spell
starfire = mkBasicSpell Starfire 6 $ \this -> Sequence [
    With $ AnyCharacter $ DealDamage 5,
    With $ CasterOf (DrawCards 1) this ]


stonetuskBoar :: Minion
stonetuskBoar = mkBasicMinion StonetuskBoar 1 1 1 [
    KeywordAbility Charge ]


stormpikeCommando :: Minion
stormpikeCommando = mkBasicMinion StormpikeCommando 5 4 2 [
    KeywordAbility $ Battlecry $ With . AnotherCharacter (DealDamage 2) ]


stormwindKnight :: Minion
stormwindKnight = mkBasicMinion StormwindKnight 4 2 5 [
    KeywordAbility Charge ]


sunwalker :: Minion
sunwalker = mkClassicMinion Sunwalker 6 4 5 [
    KeywordAbility Taunt,
    KeywordAbility DivineShield ]


warGolem :: Minion
warGolem = mkBasicMinion WarGolem 7 7 7 []


wolfRider :: Minion
wolfRider = mkBasicMinion WolfRider 3 3 1 [
    KeywordAbility Charge ]








