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
        HandCardMinion minion -> DeckCardMinion minion
        HandCardSpell spell -> DeckCardSpell spell






