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
    bloodfenRaptor,
    boulderfistOgre,
    chillwindYeti,
    coreHound,
    frostwolfGrunt,
    magmaRager,
    murlocRaider,
    noviceEngineer,
    oasisSnapjaw,
    riverCrocolisk,
    sunwalker,
    warGolem ]


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


argentSquire :: Minion
argentSquire = mkClassicMinion ArgentSquire 1 1 1 [
    KeywordAbility DivineShield]


bloodfenRaptor :: Minion
bloodfenRaptor = mkBasicMinion BloodfenRaptor 2 3 2 []


boulderfistOgre :: Minion
boulderfistOgre = mkBasicMinion BoulderfistOgre 6 6 7 []


chillwindYeti :: Minion
chillwindYeti = mkBasicMinion MurlocRaider 1 2 1 []


coreHound :: Minion
coreHound = mkBasicMinion CoreHound 7 9 5 []


frostwolfGrunt :: Minion
frostwolfGrunt = mkBasicMinion FrostwolfGrunt 2 2 2 [
    KeywordAbility Taunt]


noviceEngineer :: Minion
noviceEngineer = mkBasicMinion NoviceEngineer 2 1 1 [
    KeywordAbility $ BattleCry $ With $ Owner $ DrawCards 1]


magmaRager :: Minion
magmaRager = mkBasicMinion MagmaRager 3 5 1 []


murlocRaider :: Minion
murlocRaider = mkBasicMinion MurlocRaider 1 2 1 []


oasisSnapjaw :: Minion
oasisSnapjaw = mkBasicMinion OasisSnapjaw 4 2 7 []


riverCrocolisk :: Minion
riverCrocolisk = mkBasicMinion RiverCrocolisk 2 2 3 []


sunwalker :: Minion
sunwalker = mkClassicMinion Sunwalker 6 4 5 [
    KeywordAbility Taunt,
    KeywordAbility DivineShield]


warGolem :: Minion
warGolem = mkBasicMinion WarGolem 7 7 7 []




