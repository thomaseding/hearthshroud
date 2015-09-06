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
    toCard :: a -> Card


instance ToCard Minion where
    toCard = MinionCard


instance ToCard Spell where
    toCard = SpellCard


class Uncollectible a where
    uncollectible :: a -> a


instance Uncollectible CardMeta where
    uncollectible meta = meta { _cardMetaCollectibility = Uncollectible }


instance Uncollectible Card where
    uncollectible = \case
        MinionCard x -> MinionCard $ uncollectible x
        SpellCard x -> SpellCard $ uncollectible x


instance Uncollectible Minion where
    uncollectible minion = minion { _minionMeta = uncollectible $ _minionMeta minion }


instance Uncollectible Spell where
    uncollectible spell = spell { _spellMeta = uncollectible $ _spellMeta spell }


mkMeta :: (name -> CardName) -> Rarity -> Class -> name -> CardMeta
mkMeta f rarity clazz name = CardMeta {
    _cardMetaName = f name,
    _cardMetaClass = clazz,
    _cardMetaRarity = rarity,
    _cardMetaCollectibility = Collectible }


mkMinion' :: (name -> CardName) -> Rarity -> Class -> name -> [MinionType] -> Mana -> Attack -> Health -> [Ability] -> Minion
mkMinion' f rarity clazz name types mana attack health abilities = Minion' {
    _minionCost = ManaCost mana,
    _minionTypes = Set.fromList types,
    _minionAttack = attack,
    _minionHealth = health,
    _minionAbilities = abilities,
    _minionMeta = mkMeta f rarity clazz name }


mkSpell' :: (name -> CardName) -> Rarity -> Class -> name -> Mana -> SpellEffect -> Spell
mkSpell' f rarity clazz name mana effect = Spell' {
    _spellCost = ManaCost mana,
    _spellEffect = effect,
    _spellMeta = mkMeta f rarity clazz name }


class CharacterLike a where
    asCharacter :: Handle a -> Handle Character
    fromCharacterEnchantment :: Enchantment t Character -> Enchantment t a


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


damages :: (AsDamageSource a, CharacterLike b) => Handle a -> Handle b -> Damage -> Effect
damages source victim amount = DealDamage (asCharacter victim) amount (asDamageSource source)


when :: Condition -> Effect -> Effect
when cond effect = If cond effect DoNothing


statsDelta :: (CharacterLike a) => Attack -> Health -> Enchantment Continuous a
statsDelta attack health = fromCharacterEnchantment $ StatsDelta attack health





