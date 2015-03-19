{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


module Names where


--------------------------------------------------------------------------------


data HeroName :: * where
    BasicHeroName :: BasicHeroName -> HeroName
    deriving (Show, Eq, Ord)


data BasicHeroName
    = Malfurion
    | Rexxar
    | Jaina
    | Uther
    | Anduin
    | Valeera
    | Thrall
    | Gul'dan
    | Garrosh
    deriving (Show, Eq, Ord)


data CardName :: * where
    BasicCardName :: BasicCardName -> CardName
    deriving (Show, Eq, Ord)


data BasicCardName
    = MurlocRaider
    deriving (Show, Eq, Ord)



