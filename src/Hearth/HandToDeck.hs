{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}


module Hearth.HandToDeck where


--------------------------------------------------------------------------------


import Hearth.Model


--------------------------------------------------------------------------------


class HandToDeck h d | h -> d where
    handToDeck :: h -> d


instance HandToDeck HandCard DeckCard where
    handToDeck = \case
        HandCardMinion m -> DeckCardMinion $ handToDeck m


instance HandToDeck HandMinion DeckMinion where
    handToDeck = DeckMinion . _handMinion





