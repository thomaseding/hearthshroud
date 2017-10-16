{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Hearth.Authoring.Combinators where


--------------------------------------------------------------------------------


import qualified Data.Set as Set
import Hearth.Model
import Hearth.CardName


--------------------------------------------------------------------------------


class ToCard a where
    toCard :: a -> Card


instance ToCard MinionCard where
    toCard = CardMinion


instance ToCard SpellCard where
    toCard = CardSpell


instance ToCard WeaponCard where
    toCard = CardWeapon


instance ToCard HandCard where
    toCard = \case
        HandCardMinion minion -> CardMinion minion
        HandCardSpell spell -> CardSpell spell
        HandCardWeapon weapon -> CardWeapon weapon


instance ToCard DeckCard where
    toCard = \case
        DeckCardMinion minion -> CardMinion minion
        DeckCardSpell spell -> CardSpell spell
        DeckCardWeapon weapon -> CardWeapon weapon


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


mkMinion' :: (name -> CardName) -> Rarity -> Class -> name -> [Tribe] -> Mana -> Attack -> Health -> [Ability 'Minion] -> MinionCard
mkMinion' f rarity clazz name tribes mana attack health abilities = MinionCard {
    _minionCost = ManaCost mana,
    _minionTribes = Set.fromList tribes,
    _minionAttack = attack,
    _minionHealth = health,
    _minionAbilities = abilities,
    _minionMeta = mkMeta f rarity clazz name }


mkWeapon' :: (name -> CardName) -> Rarity -> Class -> name -> Mana -> Attack -> Durability -> [Ability 'Weapon] -> WeaponCard
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


class CharacterLike a where
    asCharacter :: Handle a -> Handle 'Character
    fromCharacterEnchantment :: Enchantment t 'Character -> Enchantment t a


instance CharacterLike 'Player where
    asCharacter = PlayerCharacter
    fromCharacterEnchantment = PlayerEnchantment


instance CharacterLike 'Minion where
    asCharacter = MinionCharacter
    fromCharacterEnchantment = MinionEnchantment


instance CharacterLike 'Character where
    asCharacter = id
    fromCharacterEnchantment = id


class AsDamageSource a where
    asDamageSource :: Handle a -> DamageSource


instance AsDamageSource 'Player where
    asDamageSource = DamagingCharacter . asCharacter


instance AsDamageSource 'Minion where
    asDamageSource = DamagingCharacter . asCharacter


instance AsDamageSource 'Character where
    asDamageSource = DamagingCharacter


instance AsDamageSource 'Spell where
    asDamageSource = DamagingSpell


class AnyEnchantmentLike (t :: Timeline) where
    asAnyEnchantment :: Enchantment t a -> AnyEnchantment a


instance AnyEnchantmentLike 'Continuous where
    asAnyEnchantment = ContinuousEnchantment


instance AnyEnchantmentLike 'Limited where
    asAnyEnchantment = LimitedEnchantment


enchant :: (AnyEnchantmentLike t) => Handle a -> Enchantment t a -> Effect
enchant h = Enchant h . asAnyEnchantment


damages :: (AsDamageSource a, CharacterLike b) => Handle a -> Handle b -> Damage -> Effect
damages source victim amount = DealDamage (asCharacter victim) amount (asDamageSource source)


when :: Condition -> Effect -> Effect
when cond effect = If cond effect DoNothing


gainAttack :: (CharacterLike a) => Attack -> Enchantment 'Continuous a
gainAttack = fromCharacterEnchantment . GainAttack


freeze :: (CharacterLike a) => Handle a -> Effect
freeze = Freeze . asCharacter





