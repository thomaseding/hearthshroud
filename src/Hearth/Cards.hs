module Hearth.Cards where


--------------------------------------------------------------------------------


import Hearth.Model
import Hearth.Names


--------------------------------------------------------------------------------


cardUniverse :: [DeckCard]
cardUniverse = map (DeckCardMinion . DeckMinion) minionUniverse


minionUniverse :: [Minion]
minionUniverse = [
    argentSquire,
    bluegillWarrior,
    bloodfenRaptor,
    boulderfistOgre,
    chillwindYeti,
    coreHound,
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
    spellbreaker,
    stonetuskBoar,
    stormpikeCommando,
    stormwindKnight,
    sunwalker,
    warGolem,
    wolfRider ]


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


allOtherCharacters :: (MinionHandle -> Effect) -> MinionHandle -> Effect
allOtherCharacters f this = With $ AllOther [this] f


targetAnotherMinion :: (MinionHandle -> Effect) -> MinionHandle -> Effect
targetAnotherMinion f this = With $ TargetNoneOf [this] f


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


dreadInfernal :: Minion
dreadInfernal = mkBasicMinion DreadInfernal 6 6 6 [
    KeywordAbility $ Battlecry $ allOtherCharacters $ DealDamage 1 ]


elvenArcher :: Minion
elvenArcher = mkBasicMinion ElvenArcher 1 1 1 [
    KeywordAbility $ Battlecry $ targetAnotherMinion $ DealDamage 1 ]


fireElemental :: Minion
fireElemental = mkBasicMinion FireElemental 6 6 5 [
    KeywordAbility $ Battlecry $ targetAnotherMinion $ DealDamage 3 ]


frostwolfGrunt :: Minion
frostwolfGrunt = mkBasicMinion FrostwolfGrunt 2 2 2 [
    KeywordAbility Taunt ]


ironbeakOwl :: Minion
ironbeakOwl = mkClassicMinion IronbeakOwl 2 2 1 [
    KeywordAbility $ Battlecry $ targetAnotherMinion $ KeywordEffect . Silence ]


ironforgeRifleman :: Minion
ironforgeRifleman = mkBasicMinion IronforgeRifleman 3 2 2 [
    KeywordAbility $ Battlecry $ targetAnotherMinion $ DealDamage 1 ]


magmaRager :: Minion
magmaRager = mkBasicMinion MagmaRager 3 5 1 []


murlocRaider :: Minion
murlocRaider = mkBasicMinion MurlocRaider 1 2 1 []


noviceEngineer :: Minion
noviceEngineer = mkBasicMinion NoviceEngineer 2 1 1 [
    KeywordAbility $ Battlecry $ \this -> With $ ControllerOf this $ DrawCards 1 ]


oasisSnapjaw :: Minion
oasisSnapjaw = mkBasicMinion OasisSnapjaw 4 2 7 []


recklessRocketeer :: Minion
recklessRocketeer = mkBasicMinion RecklessRocketeer 6 5 2 [
    KeywordAbility Charge ]


riverCrocolisk :: Minion
riverCrocolisk = mkBasicMinion RiverCrocolisk 2 2 3 []


spellbreaker :: Minion
spellbreaker = mkClassicMinion Spellbreaker 4 4 3 [
    KeywordAbility $ Battlecry $ targetAnotherMinion $ KeywordEffect . Silence ]


stonetuskBoar :: Minion
stonetuskBoar = mkBasicMinion StonetuskBoar 1 1 1 [
    KeywordAbility Charge ]


stormpikeCommando :: Minion
stormpikeCommando = mkBasicMinion StormpikeCommando 5 4 2 [
    KeywordAbility $ Battlecry $ targetAnotherMinion $ DealDamage 2 ]


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








