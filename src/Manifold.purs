module Manifold where

import Prelude (Unit, ($), (<$>), bind, return)

import Control.Monad.Eff (Eff)
import Signal (Signal, runSignal)
import Signal.Channel (CHANNEL, Channel, subscribe, channel)

-- | A `Render` function takes a state, renders a Component, and sends future
-- | actions to a Channel.
type Render eff state = Channel state -> Eff eff Unit

-- | A `Connect` function takes future state values and pipes them to a
-- | `Render` function.
type Connect eff state = Signal state ->
                         Signal (Render (channel :: CHANNEL | eff) state)

type Store state = { state :: Signal state }

runStore :: forall eff state. state ->
            Connect eff state ->
            Eff (channel :: CHANNEL | eff) (Store state)
runStore state connect = do
  stateChannel <- channel state
  let stateSignal = subscribe stateChannel
  runSignal ((_ $ stateChannel) <$> connect stateSignal)
  return $ { state: stateSignal }
