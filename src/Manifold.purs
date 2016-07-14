module Manifold (Store, StoreEffects, Update, runStore) where

import Prelude
import Control.Monad.Aff (Aff, later, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, throwException)
import Data.Foldable (foldl)
import Signal (Signal, (~>), foldp, runSignal)
import Signal.Channel (Channel, CHANNEL, send, channel, subscribe)

-- | The store consists of a Signal of State over time, a Channel for
-- | incoming Actions that trigger updates to the State, and a Channel for Aff
-- | effects that yield Arrays of Actions.
type Store action eff state =
  { stateSignal :: Signal state
  , actionChannel :: Channel (Array action)
  , effectChannel :: Channel (Aff (StoreEffects eff) (Array action)) }

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
runStore :: forall action eff state. Update action state -> state ->
            Eff (StoreEffects eff) (Store action eff state)
runStore update initialState = do
  -- A channel for Actions to be handled by the top-level Update function
  actionChannel <- channel []
  -- A channel for Aff effects that yield Actions
  effectChannel <- channel (pure [])

  let -- Flip the arguments of the update function for use with foldActions
      foldState :: (state -> action -> state)
      foldState = flip update

      -- Use the update function to update the state as actions come in
      foldActions :: (Array action) -> state -> state
      foldActions actions state = foldl foldState state actions

      -- Create a past-dependent Signal representing the state over time
      stateSignal :: Signal state
      stateSignal = foldp foldActions initialState $ subscribe actionChannel

      -- Create a signal of effects from the effectChannel
      effectSignal :: Signal (Aff (StoreEffects eff) (Array action))
      effectSignal = subscribe effectChannel

      -- Run effects and send the resulting actions to the actionChannel
      runEffect :: Aff (StoreEffects eff) (Array action) ->
                   Eff (StoreEffects eff) Unit
      runEffect effect = void $ runAff throwException (const (pure unit)) do
        actions <- later effect
        liftEff $ send actionChannel actions

  -- Pipe the effectSignal to runEffect
  runSignal $ effectSignal ~> runEffect

  -- Yield the state signal, a channel to receive actions, and a channel to
  -- receive effects
  pure $ { stateSignal, actionChannel, effectChannel }
