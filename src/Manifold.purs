module Manifold where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (foldl)
import Data.List (List(Nil))
import Signal (Signal, foldp)
import Signal.Channel (channel, subscribe, Channel, CHANNEL)

type CoreEffects eff = (channel :: CHANNEL, err :: EXCEPTION | eff)

type Store action state =
  { stateSignal :: Signal state
  , actionChannel :: Channel (List action) }

createStore :: forall action state eff.
               (action -> state -> state) ->
               state ->
               Eff (CoreEffects eff) (Store action state)
createStore update initialState = do
  actionChannel <- channel Nil
  let foldState = flip update :: (state -> action -> state)
      foldActions :: (List action) -> state -> state
      foldActions actions state = foldl foldState state actions
      stateSignal :: Signal state
      stateSignal = foldp foldActions initialState $ subscribe actionChannel
  return $ { stateSignal, actionChannel }
