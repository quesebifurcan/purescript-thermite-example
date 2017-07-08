module Lesson2.Counter
  ( counter
  , count
  , initialState
  , CounterAction(..)
  ) where

import Prelude
import Unsafe.Coerce
import Data.Lens.Internal.Forget
import React.DOM as RD
import React.DOM.Props as RP
import Thermite as T
import Data.Lens (Prism', prism', Lens, lens, over, view, set, addOver, subOver, _1, _2)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Lesson2.Button (ButtonAction(..), ButtonState, button)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Trans.Class (lift)
import Data.Newtype (wrap)
import Data.HTTP.Method (Method(..))
import Network.HTTP.Affjax (affjax, defaultRequest)
import Control.Applicative

type CounterState =
  { count :: Int
  , buttonState :: Tuple ButtonState ButtonState
  , incrementBy :: Int
  }

count :: forall a b r. Lens { count :: a | r } { count :: b | r } a b
-- count = lens _.count (_ { count = _ })
count = lens (\x -> x.count) (\x newValue -> x { count = newValue })

incrementBy :: forall a b r. Lens { incrementBy :: a | r } { incrementBy :: b | r } a b
incrementBy = lens _.incrementBy (_ { incrementBy = _ })

buttonState :: forall a b r. Lens { buttonState :: a | r } { buttonState :: b | r } a b
buttonState = lens _.buttonState (_ { buttonState = _ })

initialState :: CounterState
initialState =
  { count : 0
  , buttonState: Tuple
    { text: "Increment"
    , className: "btn btn-success"
    }
    { text: "Decrement"
    , className: "btn btn-danger"
    }
  , incrementBy: 2
  }

data CounterAction
  = Increment
  | Decrement
  | SetIncrementBy Int

render :: T.Render CounterState _ _
render dispatch _ state _ =
  [ RD.h1' [ RD.text "Lesson 2 - Separating State" ]
  , RD.p'  [ RD.text "The state is: "
           , RD.text (show state.count)
           ]
  , RD.input [
       RP.onChange (\x -> dispatch (SetIncrementBy (unsafeCoerce x).target.value))
       , RP.value (show state.incrementBy)
       ] []
  ]

-- The simple version
-- performIncrementBy state =
--   state { count = state.count + state.incrementBy }

-- performIncrementBy ::
--   forall s t a b s' t' a' b' .
--   Lens s t a b ->
--   Lens s' t' a' b' ->
--   CounterState ->
--   CounterState

-- See https://stackoverflow.com/questions/20626963/haskell-lenses-getters-and-setters ((t34 -> t33) -> t36 -> t35) ->
--performIncrementBy :: forall t24 t26 t29 t31 t33 t34 t35 t36. Semiring t33 => (Forget t33 t33 t29 -> Forget t33 t36 t31) -> (Forget t33 t33 t24 -> Forget t33 t36 t26) -> ((t34 -> t33) -> t36 -> t35) -> t36 -> t35

performIncrementBy :: forall b'' t' b' t a u state' s. Semiring a => Lens s t a b' -> Lens s t' a b'' -> ((u -> a) -> s -> state') -> s -> state'
performIncrementBy countL incrementByL countL' state =
  let incrementBy = view incrementByL state
      count = view countL state
  in
   set countL' (count + incrementBy) state

performAction :: T.PerformAction _ CounterState _ CounterAction
-- performAction Increment _ _ = void $ T.modifyState $ (over count id)
performAction Increment _ _ = do
  lift (delay (wrap 500.0))
  void $ T.modifyState $ performIncrementBy count incrementBy count
performAction Decrement _ _ = void $ T.modifyState $ subOver count 1
performAction (SetIncrementBy x) _ _ = void $ T.modifyState $ (over incrementBy (\_ -> x))

display :: T.Spec _ CounterState _ CounterAction
display = T.simpleSpec performAction render

-- map a Clicked to an Increment
_Increment :: Prism' CounterAction ButtonAction
_Increment = prism' (const Increment) $
  case _ of
    Increment -> Just Clicked
    _         -> Nothing

incrementButton :: T.Spec _ CounterState _ CounterAction
incrementButton = T.focus (buttonState <<< _1) _Increment button

-- map a Clicked to an Decrement
_Decrement :: Prism' CounterAction ButtonAction
_Decrement = prism' (const Decrement) $
  case _ of
    Decrement -> Just Clicked
    _         -> Nothing

decrementButton :: T.Spec _ CounterState _ CounterAction
decrementButton = T.focus (buttonState <<< _2) _Decrement button

counter :: T.Spec _ CounterState _ CounterAction
counter = display <> incrementButton <> decrementButton
