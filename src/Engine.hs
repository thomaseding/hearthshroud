{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Engine where


--------------------------------------------------------------------------------


type Name = String


newtype Turn = Turn Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)


newtype Cost = Cost Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)


newtype Attack = Attack Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)


newtype Armor = Armor Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)


newtype Health = Health Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)


data Effect :: * where
    Effect :: Effect
    deriving (Show, Eq, Ord)


data Ability :: * where
    Charge :: Ability
    deriving (Show, Eq, Ord)


data Enchantment :: * where
    FrozenUntil :: Turn -> Enchantment
    deriving (Show, Eq, Ord)


data Spell = Spell {
    --_spellEffects :: [SpellEffect],
    _spellName :: Name
} deriving (Show, Eq, Ord)


data Minion = Minion {
    _minionAttack :: Attack,
    _minionHealth :: Health,
    _minionName :: Name
} deriving (Show, Eq, Ord)


data BoardMinion = BoardMinion {
    _boardMinionCurrAttack :: Attack,
    _boardMinionCurrHealth :: Health,
    _boardMinionEnchantments :: [Enchantment],
    _boardMinion :: Minion
} deriving (Show, Eq, Ord)


data DeckMinion = DeckMinion {
    _deckMinion :: Minion
} deriving (Show, Eq, Ord)


data HandMinion = HandMinion {
    --_handMinionEffects :: [HandEffect]  -- Think Bolivar
    _handMinion :: Minion
} deriving (Show, Eq, Ord)


data HeroPower = HeroPower {
    _heroPowerCost :: Cost,
    _heroPowerEffects :: [Effect]
} deriving (Show, Eq, Ord)


data Hero = Hero {
    _heroAttack :: Attack,
    _heroArmor :: Armor,
    _heroHealth :: Health,
    _heroPower :: HeroPower,
    _heroName :: Name
} deriving (Show, Eq, Ord)


data BoardHero = BoardHero {
    _boardHeroCurrHealth :: Health,
    _boardHero :: Hero
} deriving (Show, Eq, Ord)


data GameState = GameState {
    _turn :: Turn,
    _players :: [Player]
} deriving (Show, Eq, Ord)


data HandCard :: * where
    HandMinionCard :: HandMinion -> HandCard
    HandSpellCard :: Spell -> HandCard
    deriving (Show, Eq, Ord)


data DeckCard :: * where
    DeckMinionCard :: DeckMinion -> DeckCard
    DeckSpellCard :: Spell -> DeckCard
    deriving (Show, Eq, Ord)


data Hand = Hand {
    _handCards :: [HandCard]
} deriving (Show, Eq, Ord)


data Deck = Deck {
    _deckCards :: [DeckCard]
} deriving (Show, Eq, Ord)


data Player = Player {
    _playerDeck :: Deck,
    _playerHand :: Hand,
    _playerMinions :: BoardMinion,
    _playerHero :: BoardHero
} deriving (Show, Eq, Ord)






















