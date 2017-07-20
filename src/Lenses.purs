module Lenses where

import Data.Lens
import Data.Lens.Record
import Data.Symbol

type Address =
  { street :: String
  , number :: Int
}

_street :: Lens' Address String
_street = prop (SProxy :: SProxy "street")

addressVal :: Address
addressVal = { street: "test", number: 15 }

streetVal = addressVal ^. _street
