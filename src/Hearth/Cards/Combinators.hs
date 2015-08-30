{-# LANGUAGE LambdaCase #-}


module Hearth.Cards.Combinators where


--------------------------------------------------------------------------------


import Hearth.Model
import Hearth.Names


--------------------------------------------------------------------------------


class ToCard a where
    toCard :: a -> DeckCard


instance ToCard Minion where
    toCard = DeckCardMinion


instance ToCard Spell where
    toCard = DeckCardSpell


class Uncollectible a where
    uncollectible :: a -> a


instance Uncollectible CardMeta where
    uncollectible meta = meta { _cardMetaCollectibility = Uncollectible }


instance Uncollectible DeckCard where
    uncollectible = \case
        DeckCardMinion x -> DeckCardMinion $ uncollectible x
        DeckCardSpell x -> DeckCardSpell $ uncollectible x


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


mkMinion' :: (name -> CardName) -> Rarity -> Class -> name -> Mana -> Attack -> Health -> [Ability] -> Minion
mkMinion' f rarity clazz name mana attack health abilities = Minion' {
    _minionCost = ManaCost mana,
    _minionAttack = attack,
    _minionHealth = health,
    _minionAbilities = abilities,
    _minionMeta = mkMeta f rarity clazz name }


mkSpell' :: (name -> CardName) -> Rarity -> Class -> name -> Mana -> SpellEffect -> Spell
mkSpell' f rarity clazz name mana effect = Spell' {
    _spellCost = ManaCost mana,
    _spellEffect = effect,
    _spellMeta = mkMeta f rarity clazz name }


