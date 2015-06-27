{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}


module Hearth.Client.Console.Render.BoardColumn (
    boardColumn
) where


--------------------------------------------------------------------------------


import Control.Error
import Control.Lens
import Data.List
import Hearth.Model
import Hearth.Cards
import Hearth.Client.Console.Render.BoardMinionsColumn


--------------------------------------------------------------------------------


boardColumn :: Player -> [String]
boardColumn = boardMinionsColumn . _playerMinions








