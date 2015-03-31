module Hearth.Cards where


--------------------------------------------------------------------------------


import Hearth.Model
import Hearth.Names


--------------------------------------------------------------------------------


cardUniverse :: [DeckCard]
cardUniverse = map (DeckCardMinion . DeckMinion) minionUniverse


minionUniverse :: [Minion]
minionUniverse = [
    bloodfenRaptor,
    boulderfistOgre,
    chillwindYeti,
    coreHound,
    magmaRager,
    murlocRaider,
    oasisSnapjaw,
    riverCrocolisk,
    warGolem ]


mkVanilla :: BasicCardName -> Mana -> Attack -> Health -> Minion
mkVanilla name mana attack health = Minion {
    _minionCost = ManaCost mana,
    _minionAttack = attack,
    _minionHealth = health,
    _minionName = BasicCardName name }


bloodfenRaptor :: Minion
bloodfenRaptor = mkVanilla BloodfenRaptor 2 3 2


boulderfistOgre :: Minion
boulderfistOgre = mkVanilla BoulderfistOgre 6 6 7


chillwindYeti :: Minion
chillwindYeti = mkVanilla MurlocRaider 1 2 1


coreHound :: Minion
coreHound = mkVanilla CoreHound 7 9 5


magmaRager :: Minion
magmaRager = mkVanilla MagmaRager 3 5 1


murlocRaider :: Minion
murlocRaider = mkVanilla MurlocRaider 1 2 1


oasisSnapjaw :: Minion
oasisSnapjaw = mkVanilla OasisSnapjaw 4 2 7


riverCrocolisk :: Minion
riverCrocolisk = mkVanilla RiverCrocolisk 2 2 3


warGolem :: Minion
warGolem = mkVanilla WarGolem 7 7 7




