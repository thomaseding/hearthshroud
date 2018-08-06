{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}


module Hearth.Client.Console.Render.BoardColumn (
    boardColumn
) where


--------------------------------------------------------------------------------


import Control.Error
import Control.Lens
import Data.List
import Hearth.Cards
import Hearth.Client.Console.Render.BoardMinionsColumn
import Hearth.Model.Authoring


--------------------------------------------------------------------------------


boardColumn :: Player -> [String]
boardColumn = boardMinionsColumn . _playerMinions


xxxxxx this is a stale file ?





