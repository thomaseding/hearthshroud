module Hearth.Cards.Classic (
    cards,
) where


--------------------------------------------------------------------------------


import Hearth.Model
import Hearth.Names
import Hearth.Names.Classic hiding (Silence)
import qualified Hearth.Names.Classic as Classic


--------------------------------------------------------------------------------


cards :: [DeckCard]
cards = [
    abomination,
    aldorPeacekeeper,
    amaniBerserker,
    arcaneGolem,
    argentCommander,
    argentProtector,
    argentSquire,
    armorsmith,
    battleRage,
    bigGameHunter,
    blessedChampion,
    brawl,
    circleOfHealing,
    coldlightOracle,
    crazedAlchemist,
    cruelTaskmaster,
    earthenRingFarseer,
    earthShock,
    equality,
    fenCreeper,
    flameImp,
    gadgetzanAuctioneer,
    grommashHellscream,
    holyFire,
    injuredBlademaster,
    innerRage,
    ironbeakOwl,
    layOnHands,
    leperGnome,
    lootHoarder,
    markOfNature,
    massDispel,
    mogu'shanWarden,
    naturalize,
    nourish,
    pitLord,
    priestessOfElune,
    pyroblast,
    rampage,
    scarletCrusader,
    shieldbearer,
    silence,
    silvermoonGuardian,
    siphonSoul,
    spellbreaker,
    stampedingKodo,
    starfall,
    sunwalker,
    taurenWarrior,
    templeEnforcer,
    twistingNether,
    wisp,
    wrath ]


--------------------------------------------------------------------------------


mkMinion :: ClassicCardName -> Mana -> Attack -> Health -> [Ability] -> DeckCard
mkMinion name mana attack health abilities = DeckCardMinion $ Minion' {
    _minionCost = ManaCost mana,
    _minionAttack = attack,
    _minionHealth = health,
    _minionAbilities = abilities,
    _minionName = ClassicCardName name }


mkSpell :: ClassicCardName -> Mana -> SpellEffect -> DeckCard
mkSpell name mana effect = DeckCardSpell $ Spell' {
    _spellCost = ManaCost mana,
    _spellEffect = effect,
    _spellName = ClassicCardName name }


--------------------------------------------------------------------------------


abomination :: DeckCard
abomination = mkMinion Abomination 5 4 4 [
    KeywordAbility Taunt,
    KeywordAbility $ Deathrattle $ \this ->
        All $ Characters [Not (MinionCharacter this)] $ \victims ->
            Effect $ ForEach victims $ \victim ->
                DealDamage victim 2 ]


amaniBerserker :: DeckCard
amaniBerserker = mkMinion AmaniBerserker 2 2 3 [
    KeywordAbility $ Enrage [] [
        StatsDelta 3 0 ]]


aldorPeacekeeper :: DeckCard
aldorPeacekeeper = mkMinion AldorPeacekeeper 3 3 3 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                A $ Minion [OwnedBy opponent] $ \target ->
                    Effect $ Enchant target [
                        ChangeStat (Left 1) ]]


arcaneGolem :: DeckCard
arcaneGolem = mkMinion ArcaneGolem 3 4 2 [
    KeywordAbility Charge,
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ GainManaCrystals opponent 1 CrystalFull ]


argentCommander :: DeckCard
argentCommander = mkMinion ArgentCommander 6 4 2 [
    KeywordAbility Charge,
    KeywordAbility DivineShield ]


argentProtector :: DeckCard
argentProtector = mkMinion ArgentProtector 2 2 2 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion [OwnedBy you, Not this] $ \target ->
                Effect $ GiveAbility target [KeywordAbility DivineShield ]]


argentSquire :: DeckCard
argentSquire = mkMinion ArgentSquire 1 1 1 [
    KeywordAbility DivineShield ]


armorsmith :: DeckCard
armorsmith = mkMinion Armorsmith 2 1 4 [
    Whenever $ TakesDamage $ \this victim ->
        OwnerOf this $ \you ->
            Effect $ When victim [OwnedBy you, IsMinion] $ GainArmor you 1 ]


battleRage :: DeckCard
battleRage = mkSpell BattleRage 2 $ \this ->
    OwnerOf this $ \you ->
        All $ Characters [Damaged, OwnedBy you] $ \friendlies ->
            Effect $ ForEach friendlies $ \_ ->
                DrawCards you 1


bigGameHunter :: DeckCard
bigGameHunter = mkMinion BigGameHunter 3 4 2 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Minion [Not this, AttackCond GreaterEqual 7] $ \target ->
            Effect $ DestroyMinion target ]


blessedChampion :: DeckCard
blessedChampion = mkSpell BlessedChampion 5 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Enchant target [
            StatsScale 2 1 ]


brawl :: DeckCard
brawl = mkSpell Brawl 5 $ \_ ->
    Effect $ Elect $ A $ Minion [] $ \survivor ->
        A $ Minion [Not survivor] $ \someNonSurvivor ->
            All $ Minions [Not survivor] $ \victims ->
                Effect $ Sequence [
                    DoNothing someNonSurvivor, -- This is because Brawl requires at least 2 minions to play.
                    ForEach victims $ \victim ->
                        DestroyMinion victim ]


circleOfHealing :: DeckCard
circleOfHealing = mkSpell CircleOfHealing 0 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ ForEach minions $ \minion ->
            RestoreHealth (MinionCharacter minion) 4


coldlightOracle :: DeckCard
coldlightOracle = mkMinion ColdlightOracle 3 2 2[
    KeywordAbility $ Battlecry $ \_ ->
        All $ Players [] $ \players ->
            Effect $ ForEach players $ \player ->
                DrawCards player 2 ]


crazedAlchemist :: DeckCard
crazedAlchemist = mkMinion CrazedAlchemist 2 2 2 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Minion [Not this] $ \target ->
            Effect $ Enchant target [
                SwapStats ]]


cruelTaskmaster :: DeckCard
cruelTaskmaster = mkMinion CruelTaskmaster 2 2 2 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Minion [Not this] $ \target ->
            Effect $ Sequence [
                DealDamage (MinionCharacter target) 1,
                Enchant target [
                    StatsDelta 2 0 ]]]


earthenRingFarseer :: DeckCard
earthenRingFarseer = mkMinion EarthenRingFarseer 3 3 3 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Character [Not (MinionCharacter this)] $ \character ->
            Effect $ RestoreHealth character 3 ]


earthShock :: DeckCard
earthShock = mkSpell EarthShock 1 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Sequence [
            Silence target,
            DealDamage (MinionCharacter target) 1 ]


equality :: DeckCard
equality = mkSpell Equality 2 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ ForEach minions $ \minion ->
            Enchant minion [
                ChangeStat (Right 1) ]


fenCreeper :: DeckCard
fenCreeper = mkMinion FenCreeper 5 3 6 [
    KeywordAbility Taunt ]


flameImp :: DeckCard
flameImp = mkMinion FlameImp 1 3 2 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DealDamage (PlayerCharacter you) 3 ]


gadgetzanAuctioneer :: DeckCard
gadgetzanAuctioneer = mkMinion GadgetzanAuctioneer 6 4 4 [
    Whenever $ SpellIsCast $ \this spell ->
        OwnerOf this $ \you ->
            Effect $ When spell [OwnedBy you] $ DrawCards you 1 ]


grommashHellscream :: DeckCard
grommashHellscream = mkMinion GrommashHellscream 8 4 9 [
    KeywordAbility Charge,
    KeywordAbility $ Enrage [] [
        StatsDelta 6 0 ]]


holyFire :: DeckCard
holyFire = mkSpell HolyFire 6 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                DealDamage target 5,
                RestoreHealth (PlayerCharacter you) 5 ]


injuredBlademaster :: DeckCard
injuredBlademaster = mkMinion InjuredBlademaster 3 4 7 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ DealDamage (MinionCharacter this) 4 ]


innerRage :: DeckCard
innerRage = mkSpell InnerRage 0 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Sequence [
            DealDamage (MinionCharacter target) 1,
            Enchant target [
                StatsDelta 2 0 ]]


ironbeakOwl :: DeckCard
ironbeakOwl = mkMinion IronbeakOwl 2 2 1 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Minion [Not this] $ \target ->
            Effect $ Silence target ]


layOnHands :: DeckCard
layOnHands = mkSpell LayOnHands 8 $ \this ->
    A $ Character [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                RestoreHealth target 6,
                DrawCards you 3 ]


leperGnome :: DeckCard
leperGnome = mkMinion LeperGnome 1 2 1 [
    KeywordAbility $ Deathrattle $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ DealDamage (PlayerCharacter opponent) 2 ]


lootHoarder :: DeckCard
lootHoarder = mkMinion LootHoarder 2 2 1 [
    KeywordAbility $ Deathrattle $ \this ->
        OwnerOf this $ \you ->
            Effect $ DrawCards you 1 ]


markOfNature :: DeckCard
markOfNature = mkSpell MarkOfNature 3 $ \_ ->
    Choice [
        A $ Minion [] $ \target ->
            Effect $ Enchant target [
                StatsDelta 4 0 ],
        A $ Minion [] $ \target ->
            Effect $ Sequence [
                Enchant target [
                    StatsDelta 0 4 ],
                GiveAbility target [
                    KeywordAbility Taunt ]]]


massDispel :: DeckCard
massDispel = mkSpell MassDispel 4 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            All $ Minions [OwnedBy opponent] $ \victims ->
                Effect $ Sequence [
                    ForEach victims $ \victim ->
                        Silence victim,
                    DrawCards you 1 ]


mogu'shanWarden :: DeckCard
mogu'shanWarden = mkMinion Mogu'shanWarden 4 1 7 [
    KeywordAbility Taunt ]


naturalize :: DeckCard
naturalize = mkSpell Naturalize 1 $ \this ->
    OwnerOf this $ \you ->
        OpponentOf you $ \opponent ->
            A $ Minion [] $ \target ->
                Effect $ Sequence [
                    DestroyMinion target,
                    DrawCards opponent 2 ]


nourish :: DeckCard
nourish = mkSpell Nourish 5 $ \this ->
    OwnerOf this $ \you ->
        Choice [
            Effect $ GainManaCrystals you 2 CrystalFull,
            Effect $ DrawCards you 3 ]


pitLord :: DeckCard
pitLord = mkMinion PitLord 4 5 6 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ DealDamage (PlayerCharacter you) 5 ]


priestessOfElune :: DeckCard
priestessOfElune = mkMinion PriestessOfElune 6 5 4 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            Effect $ RestoreHealth (PlayerCharacter you) 4 ]


pyroblast :: DeckCard
pyroblast = mkSpell Pyroblast 10 $ \_ ->
    A $ Character [] $ \target ->
        Effect $ DealDamage target 10


rampage :: DeckCard
rampage = mkSpell Rampage 2 $ \_ ->
    A $ Minion [WithMinion Damaged] $ \target ->
        Effect $ Enchant target [
            StatsDelta 3 3 ]


scarletCrusader :: DeckCard
scarletCrusader = mkMinion ScarletCrusader 3 3 1 [
    KeywordAbility DivineShield ]


shieldbearer :: DeckCard
shieldbearer = mkMinion Shieldbearer 1 0 4 [
    KeywordAbility Taunt ]


silence :: DeckCard
silence = mkSpell Classic.Silence 0 $ \_ ->
    A $ Minion [] $ \target ->
        Effect $ Silence target


silvermoonGuardian :: DeckCard
silvermoonGuardian = mkMinion SilvermoonGuardian 4 3 3 [
    KeywordAbility DivineShield ]


siphonSoul :: DeckCard
siphonSoul = mkSpell SiphonSoul 6 $ \this ->
    A $ Minion [] $ \target ->
        OwnerOf this $ \you ->
            Effect $ Sequence [
                DestroyMinion target,
                RestoreHealth (PlayerCharacter you) 3 ]


spellbreaker :: DeckCard
spellbreaker = mkMinion Spellbreaker 4 4 3 [
    KeywordAbility $ Battlecry $ \this ->
        A $ Minion [Not this] $ \target ->
            Effect $ Silence target ]


stampedingKodo :: DeckCard
stampedingKodo = mkMinion StampedingKodo 5 3 5 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                Effect $ Elect $ A $ Minion [OwnedBy opponent, AttackCond LessEqual 2] $ \victim ->
                    Effect $ DestroyMinion victim ]


starfall :: DeckCard
starfall = mkSpell Starfall 5 $ \this ->
    Choice [
        A $ Minion [] $ \target ->
            Effect $ DealDamage (MinionCharacter target) 5,
        OwnerOf this $ \you ->
            OpponentOf you $ \opponent ->
                All $ Minions [OwnedBy opponent] $ \victims ->
                    Effect $ ForEach victims $ \victim ->
                        DealDamage (MinionCharacter victim) 2 ]


sunwalker :: DeckCard
sunwalker = mkMinion Sunwalker 6 4 5 [
    KeywordAbility Taunt,
    KeywordAbility DivineShield ]


taurenWarrior :: DeckCard
taurenWarrior = mkMinion TaurenWarrior 3 2 3 [
    KeywordAbility Taunt,
    KeywordAbility $ Enrage [] [
        StatsDelta 3 0 ]]


templeEnforcer :: DeckCard
templeEnforcer = mkMinion TempleEnforcer 6 6 6 [
    KeywordAbility $ Battlecry $ \this ->
        OwnerOf this $ \you ->
            A $ Minion [OwnedBy you, Not this] $ \target ->
                Effect $ Enchant target [
                    StatsDelta 0 3 ]]


twistingNether :: DeckCard
twistingNether = mkSpell TwistingNether 8 $ \_ ->
    All $ Minions [] $ \minions ->
        Effect $ ForEach minions $ \minion ->
            DestroyMinion minion


wisp :: DeckCard
wisp = mkMinion Wisp 0 1 1 []


wrath :: DeckCard
wrath = mkSpell Wrath 2 $ \this ->
    Choice [
        A $ Minion [] $ \target ->
            Effect $ DealDamage (MinionCharacter target) 3,
        A $ Minion [] $ \target ->
            OwnerOf this $ \you ->
                Effect $ Sequence [
                    DealDamage (MinionCharacter target) 1,
                    DrawCards you 1 ]]










