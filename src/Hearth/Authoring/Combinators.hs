{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Hearth.Authoring.Combinators where


--------------------------------------------------------------------------------


import qualified Data.Set as Set
import Hearth.Model
import Hearth.CardName


--------------------------------------------------------------------------------


class ToCard a where
    toCard :: a k -> Card k


instance ToCard MinionCard where
    toCard = CardMinion


instance ToCard SpellCard where
    toCard = CardSpell


instance ToCard WeaponCard where
    toCard = CardWeapon


class Uncollectible a where
    uncollectible :: a -> a


instance Uncollectible CardMeta where
    uncollectible meta = meta { _cardMetaCollectibility = Uncollectible }


instance Uncollectible (Card k) where
    uncollectible = \case
        CardMinion x -> CardMinion $ uncollectible x
        CardSpell x -> CardSpell $ uncollectible x
        CardWeapon x -> CardWeapon $ uncollectible x


instance Uncollectible (MinionCard k) where
    uncollectible minion = minion { _minionMeta = uncollectible $ _minionMeta minion }


instance Uncollectible (SpellCard k) where
    uncollectible spell = spell { _spellMeta = uncollectible $ _spellMeta spell }


instance Uncollectible (WeaponCard k) where
    uncollectible weapon = weapon { _weaponMeta = uncollectible $ _weaponMeta weapon }


mkMeta :: (name -> CardName) -> Rarity -> Class -> name -> CardMeta
mkMeta f rarity clazz name = CardMeta {
    _cardMetaName = f name,
    _cardMetaClass = clazz,
    _cardMetaRarity = rarity,
    _cardMetaCollectibility = Collectible }


mkMinion' :: (name -> CardName) -> Rarity -> Class -> name -> [MinionType] -> Mana -> Attack -> Health -> [Ability k Minion] -> MinionCard k
mkMinion' f rarity clazz name types mana attack health abilities = MinionCard {
    _minionCost = ManaCost mana,
    _minionTypes = Set.fromList types,
    _minionAttack = attack,
    _minionHealth = health,
    _minionAbilities = abilities,
    _minionMeta = mkMeta f rarity clazz name }


mkWeapon' :: (name -> CardName) -> Rarity -> Class -> name -> Mana -> Attack -> Durability -> [Ability k Weapon] -> WeaponCard k
mkWeapon' f rarity clazz name mana attack durability abilities = WeaponCard {
    _weaponCost = ManaCost mana,
    _weaponAttack = attack,
    _weaponDurability = durability,
    _weaponAbilities = abilities,
    _weaponMeta = mkMeta f rarity clazz name }


mkSpell' :: (name -> CardName) -> Rarity -> Class -> name -> Mana -> SpellEffect k -> SpellCard k
mkSpell' f rarity clazz name mana effect = SpellCard {
    _spellCost = ManaCost mana,
    _spellEffect = effect,
    _spellMeta = mkMeta f rarity clazz name }


class CharacterLike a where
    asCharacter :: Handle a -> Handle Character
    fromCharacterEnchantment :: Enchantment k t Character -> Enchantment k t a


instance CharacterLike Player where
    asCharacter = PlayerCharacter
    fromCharacterEnchantment = PlayerEnchantment


instance CharacterLike Minion where
    asCharacter = MinionCharacter
    fromCharacterEnchantment = MinionEnchantment


instance CharacterLike Character where
    asCharacter = id
    fromCharacterEnchantment = id


class AsDamageSource a where
    asDamageSource :: Handle a -> DamageSource


instance AsDamageSource Player where
    asDamageSource = DamagingCharacter . asCharacter


instance AsDamageSource Minion where
    asDamageSource = DamagingCharacter . asCharacter


instance AsDamageSource Character where
    asDamageSource = DamagingCharacter


instance AsDamageSource Spell where
    asDamageSource = DamagingSpell


damages :: (AsDamageSource a, CharacterLike b) => Handle a -> Handle b -> Damage -> Effect k
damages source victim amount = DealDamage (asCharacter victim) amount (asDamageSource source)


when :: Condition -> Effect k -> Effect k
when cond effect = If cond effect DoNothing


statsDelta :: (CharacterLike a) => Attack -> Health -> Enchantment k Continuous a
statsDelta attack health = fromCharacterEnchantment $ StatsDelta attack health


freeze :: (CharacterLike a) => Handle a -> Effect k
freeze = Freeze . asCharacter





