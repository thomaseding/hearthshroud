{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


module Hearth.Model.Authoring (
    module Hearth.Model.Authoring,
    module Hearth.Model.Authoring.CardName,
    module Hearth.Model.Authoring.HeroName,
    module Hearth.Model.Authoring.HeroPowerName,
    ObjectType(..),
    Mana,
    Attack,
    Armor,
    Health,
    Damage,
    Durability,
    Handle,
    HandleList,
) where


--------------------------------------------------------------------------------


import Control.Lens hiding (Each)
import Hearth.Model.Authoring.CardName
import Hearth.Model.Authoring.HeroName
import Hearth.Model.Authoring.HeroPowerName
import Hearth.Model.Authoring.Internal


--------------------------------------------------------------------------------


asMinionCharacter :: Handle 'Minion' -> Handle 'Character'
asMinionCharacter = MinionCharacter


asPlayerCharacter :: Handle 'Player' -> Handle 'Character'
asPlayerCharacter = PlayerCharacter


handleList :: [Handle a] -> HandleList a
handleList = HandleList ()


toArmor :: Int -> Armor
toArmor = Armor . IdInt Nothing


toAttack :: Int -> Attack
toAttack = Attack . IdInt Nothing


toDamage :: Int -> Damage
toDamage = Damage . IdInt Nothing


toDurability :: Int -> Durability
toDurability = Durability . IdInt Nothing


toHealth :: Int -> Health
toHealth = Health . IdInt Nothing


toMana :: Int -> Mana
toMana = Mana . IdInt Nothing


data CrystalState :: * where
    CrystalFull :: CrystalState
    CrystalEmpty :: CrystalState
    CrystalTemporary :: CrystalState


data Cost :: * where
    ManaCost :: Mana -> Cost


data Selection = Targeted' | AtRandom'


data Timeline = Limited' | Continuous'


infixr 2 `Or`
infixr 3 `And`
infixr 5 `Satisfies`

data Condition :: * where
    Or :: Condition -> Condition -> Condition
    And :: Condition -> Condition -> Condition
    Satisfies :: Handle a -> [Requirement a] -> Condition


data Requirement :: ObjectType -> * where
    RequireMinion :: Requirement 'Character' -> Requirement 'Minion'
    RequirePlayer :: Requirement 'Character' -> Requirement 'Player'
    OwnedBy :: Handle 'Player' -> Requirement a
    Is :: Handle a -> Requirement a
    Not :: Handle a -> Requirement a
    IsDamageSource :: DamageSource -> Requirement a
    WithAttack :: Comparison -> Attack -> Requirement 'Character'
    WithHealth :: Comparison -> Health -> Requirement 'Character'
    Damaged :: Requirement 'Character'
    Undamaged :: Requirement 'Character'
    IsMinion :: Requirement 'Character'
    AdjacentTo :: Handle 'Minion' -> Requirement 'Minion'
    HasMaxManaCrystals :: Requirement 'Player'
    OfTribe :: Tribe -> Requirement 'Minion'
    HasCharge :: Requirement 'Minion'
    HasMinion :: [Requirement 'Minion'] -> Requirement 'Player'


data Comparison
    = Less
    | LessEqual
    | Equal
    | GreaterEqual
    | Greater


data Elect :: Selection -> * where
    OwnerOf :: Handle a -> (Handle 'Player' -> Elect s) -> Elect s
    OpponentOf :: Handle 'Player' -> (Handle 'Player' -> Elect s) -> Elect s
    A :: A s -> Elect s
    All :: All s -> Elect s
    Effect :: Effect -> Elect s
    ChooseOne' :: [Elect s] -> Elect s


data A :: Selection -> * where
    Weapon :: [Requirement 'Weapon'] -> (Handle 'Weapon' -> Elect s) -> A s
    Minion :: [Requirement 'Minion'] -> (Handle 'Minion' -> Elect s) -> A s
    Player :: [Requirement 'Player'] -> (Handle 'Player' -> Elect s) -> A s
    Character :: [Requirement 'Character'] -> (Handle 'Character' -> Elect s) -> A s


data All :: Selection -> * where
    Minions :: [Requirement 'Minion'] -> (HandleList 'Minion' -> Elect s) -> All s
    Players :: [Requirement 'Player'] -> (HandleList 'Player' -> Elect s) -> All s
    Characters :: [Requirement 'Character'] -> (HandleList 'Character' -> Elect s) -> All s


data DamageSource :: * where
    Fatigue :: DamageSource
    DamagingCharacter :: Handle 'Character' -> DamageSource
    DamagingSpell :: Handle 'Spell' -> DamageSource


data BoardLocation :: * where
    RightOf :: Handle 'Minion' -> BoardLocation
    Rightmost :: Handle 'Player' -> BoardLocation


data Effect :: * where
    Get :: Elect 'AtRandom' -> Effect
    DoNothing :: Effect
    Unreferenced :: Handle a -> Effect
    ForEachMinion :: HandleList 'Minion' -> (Handle 'Minion' -> Effect) -> Effect
    ForEachPlayer :: HandleList 'Player' -> (Handle 'Player' -> Effect) -> Effect
    ForEachCharacter :: HandleList 'Character' -> (Handle 'Character' -> Effect) -> Effect
    Sequence :: [Effect] -> Effect
    If :: Condition -> Effect -> Effect -> Effect
    DrawCards :: Handle 'Player' -> Int -> Effect
    DealDamage :: Handle 'Character' -> Damage -> DamageSource -> Effect
    Enchant :: Handle a -> AnyEnchantment a -> Effect
    GainManaCrystals :: Handle 'Player' -> Int -> CrystalState -> Effect
    DestroyMinion :: Handle 'Minion' -> Effect
    DestroyWeapon :: Handle 'Weapon' -> Effect
    EquipWeapon :: Handle 'Player' -> WeaponCard -> Effect
    RestoreHealth :: Handle 'Character' -> Health -> Effect
    RestoreToFullHealth :: Handle 'Character' -> Effect
    Transform :: Handle 'Minion' -> MinionCard -> Effect
    Silence :: Handle 'Minion' -> Effect
    GainArmor :: Handle 'Player' -> Armor -> Effect
    Freeze :: Handle 'Character' -> Effect
    Observing :: Effect -> EventListener -> Effect
    PutInHand :: Handle 'Player' -> Card -> Effect
    Summon :: MinionCard -> BoardLocation -> Effect
    RandomMissiles :: [Requirement 'Character'] -> Int -> Handle 'Spell' -> Effect
    DiscardAtRandom :: Handle 'Player' -> Effect
    TakeControl :: Handle 'Player' -> Handle 'Minion' -> Effect


data EventListener :: * where
    SpellIsCast :: (Handle 'Spell' -> Elect 'AtRandom') -> EventListener
    DamageIsDealt :: (Handle 'Character' -> Damage -> DamageSource -> Elect 'AtRandom') -> EventListener
    HealthIsRestored :: (Handle 'Character' -> Health -> Elect 'AtRandom') -> EventListener
    AtEndOfTurn :: (Handle 'Player' -> Elect 'AtRandom') -> EventListener
    Attacks :: (Handle 'Character' -> Handle 'Character' -> Elect 'AtRandom') -> EventListener


-- TODO: Need to adjust damage of minions when auras disappear (and also when they appear?)
data Aura :: * where
    AuraOwnerOf :: Handle a -> (Handle 'Player' -> Aura) -> Aura
    AuraOpponentOf :: Handle 'Player' -> (Handle 'Player' -> Aura) -> Aura
    AuraSequence :: [Aura] -> Aura
    While :: Handle a -> [Requirement a] -> Aura -> Aura
    EachMinion :: [Requirement 'Minion'] -> (Handle 'Minion' -> Aura) -> Aura
    Has :: Handle 'Minion' -> Enchantment 'Continuous' 'Minion' -> Aura
    HasAbility :: Handle 'Minion' -> Ability 'Minion' -> Aura


data Ability :: ObjectType -> * where
    ObserverMinion :: (Handle 'Minion' -> EventListener) -> Ability 'Minion'
    ObserverWeapon :: (Handle 'Weapon' -> EventListener) -> Ability 'Weapon'
    AuraMinion :: (Handle 'Minion' -> Aura) -> Ability 'Minion'
    Battlecry :: (Handle 'Minion' -> Elect 'Targeted') -> Ability 'Minion'
    Deathrattle :: (Handle 'Minion' -> Elect 'AtRandom') -> Ability 'Minion'
    ChooseOne :: (Handle 'Minion' -> [Elect 'Targeted']) -> Ability 'Minion'
    Charge :: Ability 'Minion'
    DivineShield :: Ability 'Minion'
    Enrage :: [Ability 'Minion'] -> [Enchantment 'Continuous' 'Minion'] -> Ability 'Minion'
    Taunt :: Ability 'Minion'
    SpellDamage :: Int -> Ability 'Minion'
    Windfury :: Ability 'Minion'
    Can'tAttack :: Ability 'Minion'


data TimePoint :: * where
    Delay :: Int -> TimePoint -> TimePoint
    BeginOfTurn :: TimePoint
    EndOfTurn :: TimePoint


data Enchantment :: Timeline -> ObjectType -> * where
    MinionEnchantment :: Enchantment t 'Character' -> Enchantment t 'Minion'
    PlayerEnchantment :: Enchantment t 'Character' -> Enchantment t 'Player'
    Until :: TimePoint -> Enchantment 'Continuous' a -> Enchantment 'Limited' a
    DelayedEffect :: TimePoint -> Effect -> Enchantment 'Limited' 'Minion'
    GainAttack :: Attack -> Enchantment 'Continuous' 'Character'
    GainHealth :: Health -> Enchantment 'Continuous' 'Minion'
    StatsScale :: Attack -> Health -> Enchantment 'Continuous' 'Minion'
    ChangeStat :: Either Attack Health -> Enchantment 'Continuous' 'Minion'
    SwapStats :: Enchantment 'Continuous' 'Minion'
    Grant :: Ability a -> Enchantment 'Continuous' a
    Frozen :: Enchantment 'Continuous' 'Character'
    AttackDelta :: Attack -> Enchantment 'Continuous' 'Weapon'


data AnyEnchantment :: ObjectType -> * where
    ContinuousEnchantment :: Enchantment 'Continuous' a -> AnyEnchantment a
    LimitedEnchantment :: Enchantment 'Limited' a -> AnyEnchantment a


data Tribe :: * where
    Beast :: Tribe
    Demon :: Tribe
    Dragon :: Tribe
    Mech :: Tribe
    Murloc :: Tribe
    Pirate :: Tribe
    Totem :: Tribe


data Rarity :: * where
    Free :: Rarity
    Common :: Rarity
    Rare :: Rarity
    Epic :: Rarity
    Legendary :: Rarity


data Class :: * where
    Neutral :: Class
    Paladin :: Class
    Warrior :: Class
    Shaman :: Class
    Druid :: Class
    Hunter :: Class
    Priest :: Class
    Mage :: Class
    Warlock :: Class
    Rogue :: Class


data Collectibility :: * where
    Collectible :: Collectibility
    Uncollectible :: Collectibility


data CardMeta = CardMeta {
    _cardMetaName :: CardName,
    _cardMetaClass :: Class,
    _cardMetaRarity :: Rarity,
    _cardMetaCollectibility :: Collectibility }


type SpellEffect
    = Handle 'Spell' -> Elect 'Targeted'


data SpellCard = SpellCard {
    _spellCost :: Cost,
    _spellEffect :: SpellEffect,
    _spellMeta :: CardMeta }


data WeaponCard = WeaponCard {
    _weaponCost :: Cost,
    _weaponAttack :: Attack,
    _weaponDurability :: Durability,
    _weaponAbilities :: [Ability 'Weapon'],
    _weaponMeta :: CardMeta }


data MinionCard = MinionCard {
    _minionCost :: Cost,
    _minionTribes :: [Tribe],
    _minionAttack :: Attack,
    _minionHealth :: Health,
    _minionAbilities :: [Ability 'Minion'],
    _minionMeta :: CardMeta }


type HeroPowerEffect
    = Handle 'Player' -> Elect 'Targeted'


data HeroPower = HeroPower {
    _heroPowerCost :: Cost,
    _heroPowerEffect :: HeroPowerEffect,
    _heroPowerName :: HeroPowerName }


data Hero = Hero {
    _heroAttack :: Attack,
    _heroHealth :: Health,
    _heroPower :: HeroPower,
    _heroName :: HeroName }


data Card :: * where
    CardMinion :: MinionCard -> Card
    CardSpell :: SpellCard -> Card
    CardWeapon :: WeaponCard -> Card


data Universe :: * where
    Universe :: [Card] -> Universe


unUniverse :: Universe -> [Card]
unUniverse (Universe u) = u


--------------------------------------------------------------------------------


-- Unfortunately I can't make the lenses alongside
-- their data declarations. See GHC ticket:
--   https://ghc.haskell.org/trac/ghc/ticket/10743
makeLenses ''CardMeta
makeLenses ''SpellCard
makeLenses ''WeaponCard
makeLenses ''MinionCard
makeLenses ''HeroPower
makeLenses ''Hero







