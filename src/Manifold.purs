module Manifold where

import Control.Monad.Eff (Eff)
import Prelude (return, (<$>), Unit, ($), bind)
import Signal (runSignal, Signal)
import Signal.Channel (subscribe, channel, CHANNEL, Channel)

-- | A `Render` function takes a state, renders a Component, and sends future
-- | actions to a Channel.
type Render eff state = Channel state -> Eff eff Unit

-- | A `Connect` function takes future state values and pipes them to a
-- | `Render` function.
type Connect eff state = Signal state ->
                         Signal (Render (channel :: CHANNEL | eff) state)

type Store state = { state :: Signal state }

store :: forall state. Signal state -> Store state
store stateSignal = { state: stateSignal }

createStore :: forall eff state. state ->
            Connect eff state ->
            Eff (channel :: CHANNEL | eff) (Store state)
createStore state connect = do
  stateChannel <- channel state
  let stateSignal = subscribe stateChannel
  runSignal ((_ $ stateChannel) <$> connect stateSignal)
  return $ store stateSignal
