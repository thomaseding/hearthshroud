{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Engine where


--------------------------------------------------------------------------------


data Base = Base
    deriving (Show, Eq, Ord)


newtype TurnNumber = TurnNumber Int
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


data Enchantment :: * where
    FrozenUntil :: TurnNumber -> Enchantment
    deriving (Show, Eq, Ord)


data Minion z = Minion {
    _minionAttack :: Attack,
    _minionHealth :: Health,
    _minionZoneData :: z
} deriving (Show, Eq, Ord)


type BaseMinion = Minion Base
type BoardMinion = Minion MinionBoardData


data MinionBoardData = MinionBoardData {
    _minionCurrAttack :: Attack,
    _minionCurrHealth :: Health,
    _minionEnchantments :: [Enchantment],
    _minionBase :: Minion ()
} deriving (Show, Eq, Ord)


data HeroPower = HeroPower {
    _heroPowerCost :: Cost,
    _heroPowerEffects :: [Effect]
} deriving (Show, Eq, Ord)


data Hero z = Hero {
    _heroAttack :: Attack,
    _heroArmor :: Armor,
    _heroHealth :: Health,
    _heroPower :: HeroPower,
    _heroZoneData :: z
} deriving (Show, Eq, Ord)


type BaseHero = Hero Base
type BoardHero = Hero HeroBoardData


data HeroBoardData = HeroBoardData {
    _heroCurrHealth :: Health,
    _heroBase :: BaseHero
} deriving (Show, Eq, Ord)




