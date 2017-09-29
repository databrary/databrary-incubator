{-# LANGUAGE GADTs, TupleSections, QuasiQuotes, TypeOperators #-}
module Databrary.HTTP.Path.Parser
  ( PathParser
  , (>$<)
  , (</>)
  , (</>>)
  , (</>>>)
  , (>/>)
  , (</<)
  , (|/|)
  , pathMaybe
  , (=/=)
  ) where

import qualified Data.Invertible as I
import Web.Route.Invertible

type PathParser = Path

infixr 2 </>, </>>, </>>>, >/>, </<
(</>) :: PathParser a -> PathParser b -> PathParser (a, b)
(</>) = (>*<)

(</>>) :: PathParser a -> PathParser (b, c) -> PathParser (a, b, c)
(</>>) = (>*<<)

(</>>>) :: PathParser a -> PathParser (b, c, d) -> PathParser (a, b, c, d)
(</>>>) = (>*<<<)

(>/>) :: PathParser () -> PathParser a -> PathParser a
(>/>) = (*<)

(</<) :: PathParser a -> PathParser () -> PathParser a
(</<) = (>*)

infix 3 |/|, =/=
(|/|) :: PathParser a -> PathParser b -> PathParser (Either a b)
(|/|) = (>|<)

-- empty-biased version of 'optionalI'
pathMaybe :: PathParser a -> PathParser (Maybe a)
pathMaybe p = I.rgt >$< (unit |/| p)

(=/=) :: Eq a => a -> PathParser a -> PathParser a
(=/=) a p = I.fromMaybe a >$< pathMaybe p
