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
    consecration,
    coreHound,
    darkscaleHealer,
    dreadInfernal,
    elvenArcher,
    fanOfKnives,
    fireball,
    fireElemental,
    flamestrike,
    frostwolfGrunt,
    gnomishInventor,
    goldshireFootman,
    holyNova,
    holySmite,
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


mkMinion :: BasicCardName -> Mana -> Attack -> Health -> [Ability] -> DeckCard
mkMinion name mana attack health abilities = DeckCardMinion $ Minion {
    _minionCost = ManaCost mana,
    _minionAttack = attack,
    _minionHealth = health,
    _minionAbilities = abilities,
    _minionName = BasicCardName name }


mkSpell :: BasicCardName -> Mana -> SpellEffect -> DeckCard
mkSpell name mana effect = DeckCardSpell $ Spell {
    _spellCost = ManaCost mana,
    _spellEffect = effect,
    _spellName = BasicCardName name }


--------------------------------------------------------------------------------


assassinate :: DeckCard
assassinate = mkSpell Assassinate 0 $ \_ ->
    Targeted $ AnyMinion $ \target ->
        Effect $ DestroyMinion target


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
    Effect $ With $ Unique $ CasterOf this $ \controller ->
        With $ Unique $ OpponentOf controller $ \opponent ->
            With $ All $ CharactersOf opponent $ \enemies ->
                ForEach enemies $ \enemy ->
                    DealDamage enemy 2


coreHound :: DeckCard
coreHound = mkMinion CoreHound 7 9 5 []


darkscaleHealer :: DeckCard
darkscaleHealer = mkMinion DarkscaleHealer 5 4 5 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ With $ Unique $ ControllerOf this $ \controller ->
            With $ All $ CharactersOf controller $ \friendlies ->
                ForEach friendlies $ \friendly ->
                    RestoreHealth friendly 2 ]


dreadInfernal :: DeckCard
dreadInfernal = mkMinion DreadInfernal 6 6 6 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ With $ All $ OtherCharacters (MinionCharacter this) $ \victims ->
            ForEach victims $ \victim ->
                DealDamage victim 1 ]


elvenArcher :: DeckCard
elvenArcher = mkMinion ElvenArcher 1 1 1 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ AnotherCharacter (MinionCharacter this) $ \target ->
            Effect $ DealDamage target 1 ]


fanOfKnives :: DeckCard
fanOfKnives = mkSpell Consecration 4 $ \this ->
    Effect $ With $ Unique $ CasterOf this $ \controller ->
        With $ Unique $ OpponentOf controller $ \opponent ->
            With $ All $ MinionsOf opponent $ \enemies ->
                Sequence [
                    ForEach enemies $ \enemy ->
                        DealDamage (MinionCharacter enemy) 1,
                    DrawCards controller 1 ]


fireball :: DeckCard
fireball = mkSpell Fireball 4 $ \_ ->
    Targeted $ AnyCharacter $ \target ->
        Effect $ DealDamage target 6


fireElemental :: DeckCard
fireElemental = mkMinion FireElemental 6 6 5 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ AnotherCharacter (MinionCharacter this) $ \target ->
            Effect $ DealDamage target 3 ]


flamestrike :: DeckCard
flamestrike = mkSpell Flamestrike 7 $ \this ->
    Effect $ With $ Unique $ CasterOf this $ \controller ->
        With $ Unique $ OpponentOf controller $ \opponent ->
            With $ All $ MinionsOf opponent $ \victims ->
                ForEach victims $ \victim ->
                    DealDamage (MinionCharacter victim) 4


frostwolfGrunt :: DeckCard
frostwolfGrunt = mkMinion FrostwolfGrunt 2 2 2 [
    KeywordAbility Taunt ]


gnomishInventor :: DeckCard
gnomishInventor = mkMinion GnomishInventor 4 2 4 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ With $ Unique $ ControllerOf this $ \controller ->
            DrawCards controller 1 ]


goldshireFootman :: DeckCard
goldshireFootman = mkMinion GoldshireFootman 1 1 2 [
    KeywordAbility Taunt ]


holyNova :: DeckCard
holyNova = mkSpell HolyNova 5 $ \this ->
    Effect $ With $ Unique $ CasterOf this $ \controller ->
        With $ Unique $ OpponentOf controller $ \opponent ->
            With $ All $ CharactersOf controller $ \friendlies ->
                With $ All $ CharactersOf opponent $ \enemies ->
                    Sequence [
                        ForEach enemies $ \enemy ->
                            DealDamage enemy 2,
                        ForEach friendlies $ \friendly ->
                            RestoreHealth friendly 2 ]


holySmite :: DeckCard
holySmite = mkSpell HolySmite 1 $ \_ ->
    Targeted $ AnyCharacter $ \target ->
        Effect $ DealDamage target 2


innervate :: DeckCard
innervate = mkSpell Innervate 0 $ \this ->
    Effect $ With $ Unique $ CasterOf this $ \caster ->
        Sequence $ replicate 2 $ GainManaCrystal CrystalTemporary caster


ironforgeRifleman :: DeckCard
ironforgeRifleman = mkMinion IronforgeRifleman 3 2 2 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ AnotherCharacter (MinionCharacter this) $ \target ->
            Effect $ DealDamage target 1 ]


lordOfTheArena :: DeckCard
lordOfTheArena = mkMinion LordOfTheArena 6 6 5 [
    KeywordAbility Taunt ]


magmaRager :: DeckCard
magmaRager = mkMinion MagmaRager 3 5 1 []


moonfire :: DeckCard
moonfire = mkSpell Moonfire 0 $ \_ ->
    Targeted $ AnyCharacter $ \target ->
        Effect $ DealDamage target 1


murlocRaider :: DeckCard
murlocRaider = mkMinion MurlocRaider 1 2 1 []


nightblade :: DeckCard
nightblade = mkMinion Nightblade 5 4 4 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ With $ Unique $ ControllerOf this $ \controller ->
            With $ Unique $ OpponentOf controller $ \opponent ->
                DealDamage (PlayerCharacter opponent) 2 ]


noviceEngineer :: DeckCard
noviceEngineer = mkMinion NoviceEngineer 2 1 1 [
    KeywordAbility $ Battlecry $ \this ->
        Effect $ With $ Unique $ ControllerOf this $ \controller ->
            DrawCards controller 1 ]


oasisSnapjaw :: DeckCard
oasisSnapjaw = mkMinion OasisSnapjaw 4 2 7 []


recklessRocketeer :: DeckCard
recklessRocketeer = mkMinion RecklessRocketeer 6 5 2 [
    KeywordAbility Charge ]


riverCrocolisk :: DeckCard
riverCrocolisk = mkMinion RiverCrocolisk 2 2 3 []


sen'jinShieldmasta :: DeckCard
sen'jinShieldmasta = mkMinion Sen'jinShieldmasta 4 3 5 [
    KeywordAbility Taunt ]


shatteredSunCleric :: DeckCard
shatteredSunCleric = mkMinion ShatteredSunCleric 3 3 2 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ AnotherFriendlyMinion this $ \target ->
            Effect $ Enchant target [StatsDelta 1 1]]


silverbackPatriarch :: DeckCard
silverbackPatriarch = mkMinion SilverbackPatriarch 3 1 4 [
    KeywordAbility Taunt ]


starfire :: DeckCard
starfire = mkSpell Starfire 6 $ \this ->
    Targeted $ AnyCharacter $ \target ->
        Effect $ With $ Unique $ CasterOf this $ \caster ->
            Sequence [
                DealDamage target 5,
                DrawCards caster 1 ]


stonetuskBoar :: DeckCard
stonetuskBoar = mkMinion StonetuskBoar 1 1 1 [
    KeywordAbility Charge ]


stormpikeCommando :: DeckCard
stormpikeCommando = mkMinion StormpikeCommando 5 4 2 [
    KeywordAbility $ Battlecry $ \this ->
        Targeted $ AnotherCharacter (MinionCharacter this) $ \target ->
            Effect $ DealDamage target 2 ]


stormwindKnight :: DeckCard
stormwindKnight = mkMinion StormwindKnight 4 2 5 [
    KeywordAbility Charge ]


swipe :: DeckCard
swipe = mkSpell Swipe 4 $ \_ ->
    Targeted $ AnyEnemy $ \target ->
        Effect $ With $ All $ OtherEnemies target $ \others ->
            Sequence [
                DealDamage target 4,
                ForEach others $ \other ->
                    DealDamage other 1 ]


theCoin :: DeckCard
theCoin = mkSpell TheCoin 0 $ \this ->
    Effect $ With $ Unique $ CasterOf this $ \caster ->
        GainManaCrystal CrystalTemporary caster


voodooDoctor :: DeckCard
voodooDoctor = mkMinion VoodooDoctor 1 2 1 [
    KeywordAbility $ Battlecry $ \_ ->
        Targeted $ AnyCharacter $ \character ->
            Effect $ RestoreHealth character 2 ]


warGolem :: DeckCard
warGolem = mkMinion WarGolem 7 7 7 []


wildGrowth :: DeckCard
wildGrowth = mkSpell WildGrowth 2 $ \this ->
    Effect $ With $ Unique $ CasterOf this $ \caster ->
        GainManaCrystal CrystalEmpty caster


wolfRider :: DeckCard
wolfRider = mkMinion WolfRider 3 3 1 [
    KeywordAbility Charge ]








