module Manifold where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (foldl)
import Data.List (List(Nil))
import Signal (Signal, foldp)
import Signal.Channel (channel, subscribe, Channel, CHANNEL)

-- | The set of effects that need to be allowed when using `runStore`. Extend
-- | this type with your own effects, for example:
-- |
-- | ```purescript
-- | type StoreEffects = (console :: CONSOLE, dom :: DOM)
-- |
-- | main :: State -> Eff (CoreEffects StoreEffects) (Store Action State)
-- | main state = do
-- |   -- ...
-- | ```
type CoreEffects eff = (channel :: CHANNEL, err :: EXCEPTION | eff)

-- | The store consists of a Signal of State over time and a Channel for
-- | incoming Actions that trigger updates to the State.
type Store action state =
  { stateSignal :: Signal state
  , actionChannel :: Channel (List action) }

type Update action state = (action -> state -> state)

-- | Creates a store and starts listening for actions. Takes the top-level
-- | Update function and an initial State value.
runStore :: forall action state eff.
            Update action state ->
            state ->
            Eff (CoreEffects eff) (Store action state)
runStore update initialState = do
  -- A channel for Actions to be handled by the top-level Update function
  actionChannel <- channel Nil
  -- A channel for Aff effects that yield Actions
  affectChannel <- channel Nil
  let -- Flip the arguments of the update function for use with foldActions
      foldState :: (state -> action -> state)
      foldState = flip update
      -- Use the update function to update the state as actions come in
      foldActions :: (List action) -> state -> state
      foldActions actions state = foldl foldState state actions
      -- Create a past-dependent Signal representing the state over time
      stateSignal :: Signal state
      stateSignal = foldp foldActions initialState $ subscribe actionChannel
  -- Yield the state signal and a channel to receive actions
  return $ { stateSignal, actionChannel }
