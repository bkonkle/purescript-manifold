module Manifold where

import Prelude (class Show, Unit, ($), bind, map, pure)

import Control.Monad.Aff (Aff, launchAff, later)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (foldl, sequence_)
import Data.List (List(Nil))
import Signal (Signal, (~>), foldp, runSignal)
import Signal.Channel (CHANNEL, Channel, channel, subscribe, send)

class (Show action) <= Action action where
  payload :: forall b. action -> b

type AsyncAction a eff = (Action a) => Aff (channel :: CHANNEL | eff) (List a)

type CoreEffects eff = (channel :: CHANNEL, err :: EXCEPTION | eff)

type Store action state eff = (Action action) =>
  { state :: (Signal state)
  , channels ::
    { actions :: Channel (List action)
    , affects :: Channel (List (AsyncAction action eff)) } }

-- | Launch the affect and pipe the resulting list of actions into the given
-- | channel.
mapAffect :: forall action eff. (Action action) =>
             Channel (List action) ->
             AsyncAction action eff ->
             Eff (CoreEffects eff) Unit
mapAffect chan affect = launchAff $ do
  actions <- later affect
  liftEff $ send chan actions

store :: forall action state eff. (Action action) =>
         Signal state ->
         Channel (List action) ->
         Channel (List (AsyncAction action eff)) ->
         Store action state eff
store stateSignal actionChannel affectChannel =
  { state: stateSignal
  , channels:
    { actions: actionChannel
    , affects: affectChannel } }

-- | Initialize state management
createStore :: forall action state eff. (Action action) =>
             (action -> state -> state) -> state ->
             Eff (CoreEffects eff) (Store action state eff)
createStore update initialState = do
  -- | Set up channels for actions and async effects
  actionChannel <- channel Nil
  affectChannel <- channel Nil
  let foldState state action = update action state
      foldActions actions state = foldl foldState state actions
      stateSignal = foldp foldActions initialState $ subscribe actionChannel
      effectSignal = (subscribe affectChannel) ~> map (mapAffect actionChannel)
  runSignal $ effectSignal ~> sequence_
  pure $ store stateSignal actionChannel affectChannel
