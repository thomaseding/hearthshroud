module Hearth.Combinator.Authoring.RebindableSyntax (
    fromInteger,
    --fromRational,
    --(==),
    --(-),
    --(>=),
    --negate,
    ifThenElse,
    --(>>=),
    --(>>),
    --fail,
    --arr,
    --(>>>),
    --first,
    --app,
    --(|||),
    --loop
) where


import Data.FromInt (FromInt(fromInt))
import Hearth.Model.Authoring
import Prelude (Integer, (.))

import qualified Prelude


--------------------------------------------------------------------------------


fromInteger :: (FromInt a) => Integer -> a
fromInteger = fromInt . Prelude.fromInteger


ifThenElse :: Condition -> Effect -> Effect -> Effect
ifThenElse = If



