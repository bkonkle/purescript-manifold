module Manifold where

import Prelude

import Control.Monad.Aff (Aff, later, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (foldl)
import Data.Sequence (Seq, empty)
import Signal (runSignal, Signal, (~>), foldp)
import Signal.Channel (Channel, CHANNEL, send, channel, subscribe)

-- | The store consists of a Signal of State over time and a Channel for
-- | incoming Actions that trigger updates to the State.
type Store action state eff =
  { stateSignal :: Signal state
  , actionChannel :: Channel (Seq action)
  , effectChannel :: Channel (Aff (StoreEffects eff) (Seq action)) }

-- | An update function takes an action and an initial state, and returns an
-- | updated state.
type Update action state = (action -> state -> state)

-- | The set of effects that need to be allowed when using `runStore`. Extend
-- | this type with your own effects, for example:
-- |
-- | ```purescript
-- | type AppEffects = (console :: CONSOLE, dom :: DOM)
-- |
-- | main :: State -> Eff (StoreEffects AppEffects) (Store Action State)
-- | main state = do
-- |   -- ...
-- | ```
type StoreEffects eff = (channel :: CHANNEL, err :: EXCEPTION | eff)

-- | Creates a store and starts listening for actions. Takes the top-level
-- | Update function and an initial State value.
runStore :: forall action state eff. Update action state -> state ->
            Eff (StoreEffects eff) (Store action state eff)
runStore update initialState = do
  -- A channel for Actions to be handled by the top-level Update function
  actionChannel <- channel empty
  -- A channel for Aff effects that yield Actions
  effectChannel <- channel (pure empty)

  let -- Flip the arguments of the update function for use with foldActions
      foldState :: (state -> action -> state)
      foldState = flip update

      -- Use the update function to update the state as actions come in
      foldActions :: (Seq action) -> state -> state
      foldActions actions state = foldl foldState state actions

      -- Create a past-dependent Signal representing the state over time
      stateSignal :: Signal state
      stateSignal = foldp foldActions initialState $ subscribe actionChannel

      -- Create a signal of effects from the effectChannel
      effectSignal :: Signal (Aff (StoreEffects eff) (Seq action))
      effectSignal = subscribe effectChannel

      -- Run effects and send the resulting actions to the actionChannel
      runEffect :: Channel (Seq action) ->
                   Aff (StoreEffects eff) (Seq action) ->
                   Eff (StoreEffects eff) Unit
      runEffect chan effect = launchAff do
        actions <- later effect
        liftEff $ send chan actions

  -- Pipe the effectSignal to runEffect
  runSignal $ effectSignal ~> runEffect actionChannel

  -- Yield the state signal, a channel to receive actions, and a channel to receive effects
  return $ { stateSignal, actionChannel, effectChannel }
