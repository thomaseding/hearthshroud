{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Hearth.Client.Console.Doc.Display (
    module Data.String,
    module Data.Text.Prettyprint.Doc,
    module Data.Text.Prettyprint.Doc.Render.Terminal,
    Display(..),
    labelColor,
    valueColor,
    dashSeparator,
) where


--------------------------------------------------------------------------------


import Data.String
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal


--------------------------------------------------------------------------------


data Display :: * where
    Label :: Display
    Value :: Display
    Separator :: Display
    MinionName :: {
        _hasDivineShield :: Bool }
        -> Display
    MinionIndex :: Display
    MinionStats :: Display
    DocAttack :: Display
    DocHealth :: {
        _isInjured :: Bool }
        -> Display


toAnsiStyle :: Display -> [AnsiStyle]
toAnsiStyle = \case
    Label -> [colorDull Green]
    Value -> [colorDull Green]
    Separator -> [color Green]
    x @ MinionName {} -> case _hasDivineShield x of
        True -> [color Red, bgColor Yellow]
        False -> [color Green]
    MinionIndex -> [colorDull Green]
    MinionIndex -> [colorDull White]
    DocAttack -> [color Black]
    x @ DocHealth {} -> case _isInjured x of
        True -> [color Red]
        False -> [color Black]


labelColor :: Display -> AnsiStyle]
labelColor = colorDull Green


separatorColor :: Display -> AnsiStyle
separatorColor = colorDull Green


valueColor :: Display -> AnsiStyle
valueColor = color Green


dashSeparator :: Doc Display
dashSeparator = annotate Separator "-----"


