module Lenses where

import Prelude
import Data.Lens
import Data.Lens.Record
import Data.Symbol
import Data.Lens.Iso.Newtype
import Data.Newtype
import Control.Bind ((=<<))

newtype Street = Street String
derive instance streetNewtype :: Newtype Street _

type Address =
  { street :: Street
  , number :: Int
  , additionalLines :: Array String
}

type Person =
  { name :: String
  , addresses :: Array Address
  }

_streetIso :: Iso Street Street String String
_streetIso = _Newtype

_street :: Lens' Address Street
_street = prop (SProxy :: SProxy "street")

_additionalLines :: Lens' Address (Array String)
_additionalLines = prop (SProxy :: SProxy "additionalLines")

_number :: Lens' Address Int
_number = prop (SProxy :: SProxy "number")

_streetDirect :: Lens' Address String
_streetDirect = _street <<< _streetIso

addressVal :: Address
addressVal = { street: Street "test", number: 15, additionalLines: [] }

streetVal :: String
streetVal = addressVal ^. _street ^. _streetIso

setNumber :: Address -> Int -> Address
setNumber address newNum =
  address # _number .~ newNum

addLine :: Address -> String -> Address
addLine address line =
  address # _additionalLines <>~ [line]
