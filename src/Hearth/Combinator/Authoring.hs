{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Hearth.Combinator.Authoring where


--------------------------------------------------------------------------------


import Hearth.Model.Authoring
import Prelude hiding (sequence)


--------------------------------------------------------------------------------


class ToCard a where
    toCard :: a -> Card


instance ToCard MinionCard where
    toCard = CardMinion


instance ToCard SpellCard where
    toCard = CardSpell


instance ToCard WeaponCard where
    toCard = CardWeapon


--------------------------------------------------------------------------------


class FromInt a where
    fromInt :: Int -> a


instance FromInt Durability where
    fromInt = toDurability


instance FromInt Damage where
    fromInt = toDamage


instance FromInt Mana where
    fromInt = toMana


instance FromInt Health where
    fromInt = toHealth


instance FromInt Armor where
    fromInt = toArmor


instance FromInt Attack where
    fromInt = toAttack


_0 :: (FromInt a) => a
_0 = fromInt 0


_1 :: (FromInt a) => a
_1 = fromInt 1


_2 :: (FromInt a) => a
_2 = fromInt 2


_3 :: (FromInt a) => a
_3 = fromInt 3


_4 :: (FromInt a) => a
_4 = fromInt 4


_5 :: (FromInt a) => a
_5 = fromInt 5


_6 :: (FromInt a) => a
_6 = fromInt 6


_7 :: (FromInt a) => a
_7 = fromInt 7


_8 :: (FromInt a) => a
_8 = fromInt 8


_9 :: (FromInt a) => a
_9 = fromInt 9


_10 :: (FromInt a) => a
_10 = fromInt 10


_11 :: (FromInt a) => a
_11 = fromInt 11


_12 :: (FromInt a) => a
_12 = fromInt 12


_13 :: (FromInt a) => a
_13 = fromInt 13


_14 :: (FromInt a) => a
_14 = fromInt 14


_15 :: (FromInt a) => a
_15 = fromInt 15


_16 :: (FromInt a) => a
_16 = fromInt 16


_17 :: (FromInt a) => a
_17 = fromInt 17


_18 :: (FromInt a) => a
_18 = fromInt 18


_19 :: (FromInt a) => a
_19 = fromInt 19


_20 :: (FromInt a) => a
_20 = fromInt 20


_21 :: (FromInt a) => a
_21 = fromInt 21


_22 :: (FromInt a) => a
_22 = fromInt 22


_23 :: (FromInt a) => a
_23 = fromInt 23


_24 :: (FromInt a) => a
_24 = fromInt 24


_25 :: (FromInt a) => a
_25 = fromInt 25


_26 :: (FromInt a) => a
_26 = fromInt 26


_27 :: (FromInt a) => a
_27 = fromInt 27


_28 :: (FromInt a) => a
_28 = fromInt 28


_29 :: (FromInt a) => a
_29 = fromInt 29


_30 :: (FromInt a) => a
_30 = fromInt 30


--------------------------------------------------------------------------------


class Uncollectible a where
    uncollectible :: a -> a


instance Uncollectible CardMeta where
    uncollectible meta = meta { _cardMetaCollectibility = Uncollectible }


instance Uncollectible (Card) where
    uncollectible = \case
        CardMinion x -> CardMinion $ uncollectible x
        CardSpell x -> CardSpell $ uncollectible x
        CardWeapon x -> CardWeapon $ uncollectible x


instance Uncollectible (MinionCard) where
    uncollectible minion = minion { _minionMeta = uncollectible $ _minionMeta minion }


instance Uncollectible (SpellCard) where
    uncollectible spell = spell { _spellMeta = uncollectible $ _spellMeta spell }


instance Uncollectible (WeaponCard) where
    uncollectible weapon = weapon { _weaponMeta = uncollectible $ _weaponMeta weapon }


mkMeta :: (name -> CardName) -> Rarity -> Class -> name -> CardMeta
mkMeta f rarity clazz name = CardMeta {
    _cardMetaName = f name,
    _cardMetaClass = clazz,
    _cardMetaRarity = rarity,
    _cardMetaCollectibility = Collectible }


mkMinion' :: (name -> CardName) -> Rarity -> Class -> name -> [Tribe] -> Mana -> Attack -> Health -> [Ability 'Minion'] -> MinionCard
mkMinion' f rarity clazz name tribes mana attack health abilities = MinionCard {
    _minionCost = ManaCost mana,
    _minionTribes = tribes,
    _minionAttack = attack,
    _minionHealth = health,
    _minionAbilities = abilities,
    _minionMeta = mkMeta f rarity clazz name }


mkWeapon' :: (name -> CardName) -> Rarity -> Class -> name -> Mana -> Attack -> Durability -> [Ability 'Weapon'] -> WeaponCard
mkWeapon' f rarity clazz name mana attack durability abilities = WeaponCard {
    _weaponCost = ManaCost mana,
    _weaponAttack = attack,
    _weaponDurability = durability,
    _weaponAbilities = abilities,
    _weaponMeta = mkMeta f rarity clazz name }


mkSpell' :: (name -> CardName) -> Rarity -> Class -> name -> Mana -> SpellEffect -> SpellCard
mkSpell' f rarity clazz name mana effect = SpellCard {
    _spellCost = ManaCost mana,
    _spellEffect = effect,
    _spellMeta = mkMeta f rarity clazz name }


class CharacterLike (a :: ObjectType) where
    asCharacter :: Handle a -> Handle 'Character'
    fromCharacterEnchantment :: Enchantment t 'Character' -> Enchantment t a


instance CharacterLike 'Player' where
    asCharacter = asPlayerCharacter
    fromCharacterEnchantment = PlayerEnchantment


instance CharacterLike 'Minion' where
    asCharacter = asMinionCharacter
    fromCharacterEnchantment = MinionEnchantment


instance CharacterLike 'Character' where
    asCharacter = id
    fromCharacterEnchantment = id


class AsDamageSource (a :: ObjectType) where
    asDamageSource :: Handle a -> DamageSource


instance AsDamageSource 'Player' where
    asDamageSource = DamagingCharacter . asCharacter


instance AsDamageSource 'Minion' where
    asDamageSource = DamagingCharacter . asCharacter


instance AsDamageSource 'Character' where
    asDamageSource = DamagingCharacter


instance AsDamageSource 'Spell' where
    asDamageSource = DamagingSpell


class AnyEnchantmentLike (t :: Timeline) where
    asAnyEnchantment :: Enchantment t a -> AnyEnchantment a


instance AnyEnchantmentLike 'Continuous' where
    asAnyEnchantment = ContinuousEnchantment


instance AnyEnchantmentLike 'Limited' where
    asAnyEnchantment = LimitedEnchantment


enchant :: (AnyEnchantmentLike t) => Handle a -> Enchantment t a -> Effect
enchant h = Enchant h . asAnyEnchantment


damages :: (AsDamageSource a, CharacterLike b) => Handle a -> Handle b -> Damage -> Effect
damages source victim amount = DealDamage (asCharacter victim) amount (asDamageSource source)


when :: Condition -> Effect -> Effect
when cond effect = If cond effect DoNothing


gainAttack :: (CharacterLike a) => Attack -> Enchantment 'Continuous' a
gainAttack = fromCharacterEnchantment . GainAttack


freeze :: (CharacterLike a) => Handle a -> Effect
freeze = Freeze . asCharacter


class AsObserver (a :: ObjectType) where
    observer :: (Handle a -> EventListener) -> Ability a

instance AsObserver 'Minion' where
    observer = ObserverMinion


class AsAura (a :: ObjectType) where
    aura :: (Handle a -> Aura) -> Ability a

instance AsAura 'Minion' where
    aura = AuraMinion


class AuraElect (a :: *) where
    ownerOf :: Handle o -> (Handle 'Player' -> a) -> a
    opponentOf :: Handle 'Player' -> (Handle 'Player' -> a) -> a

instance AuraElect (Elect s) where
    ownerOf = OwnerOf
    opponentOf = OpponentOf

instance AuraElect Aura where
    ownerOf = AuraOwnerOf
    opponentOf = AuraOpponentOf


class AsSequence (a :: *) where
    sequence :: [a] -> a

instance AsSequence Effect where
    sequence = Sequence

instance AsSequence Aura where
    sequence = AuraSequence




class AsForEach (a :: ObjectType) where
    forEach :: HandleList a -> (Handle a -> Effect) -> Effect

instance AsForEach 'Minion' where
    forEach = ForEachMinion

instance AsForEach 'Player' where
    forEach = ForEachPlayer

instance AsForEach 'Character' where
    forEach = ForEachCharacter



class AsDestroy (a :: ObjectType) where
    destroy :: Handle a -> Effect

instance AsDestroy 'Minion' where
    destroy = DestroyMinion

instance AsDestroy 'Weapon' where
    destroy = DestroyWeapon


class CharacterRequirment (a :: ObjectType) where
    withAttack :: Comparison -> Attack -> Requirement a
    withHealth :: Comparison -> Health -> Requirement a
    damaged :: Requirement a
    undamaged :: Requirement a

instance CharacterRequirment 'Character' where
    withAttack = WithAttack
    withHealth = WithHealth
    damaged = Damaged
    undamaged = Undamaged

instance CharacterRequirment 'Minion' where
    withAttack c = RequireMinion . WithAttack c
    withHealth c = RequireMinion . WithHealth c
    damaged = RequireMinion Damaged
    undamaged = RequireMinion Undamaged

instance CharacterRequirment 'Player' where
    withAttack c = RequirePlayer . WithAttack c
    withHealth c = RequirePlayer . WithHealth c
    damaged = RequirePlayer Damaged
    undamaged = RequirePlayer Undamaged



