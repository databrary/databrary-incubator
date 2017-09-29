module Databrary.HTTP.Path
  ( PathParameter
  , PathValues
  ) where

import Web.Route.Invertible
import Web.Route.Invertible.Internal

type PathParameter = Parameter PathString
type PathValues = [PlaceholderValue PathString]
