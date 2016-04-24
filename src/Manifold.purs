module Manifold (class Action, AsyncAction, initState, payload) where

import Prelude (class Show, Unit, ($), bind, map, pure)

import Control.Monad.Aff (Aff, launchAff, later)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (foldl, sequence_)
import Data.List (List(Nil), (:), fromFoldable, reverse, singleton)
import Data.Maybe.Unsafe (fromJust)
import Signal (Signal, (~>), mergeMany, foldp, runSignal)
import Signal.Channel (CHANNEL, Channel, channel, subscribe, send)

class (Show action) <= Action action where
  payload :: forall b. action -> b

type AsyncAction a eff = (Action a) => Aff (channel :: CHANNEL | eff) (List a)

type Config action state eff = (Action action) =>
  { actions :: Array (Signal action)
  , effects :: Array (Signal (AsyncAction action eff))
  , update :: action -> state -> state
  , initialState :: state }

type CoreEffects eff = (channel :: CHANNEL, err :: EXCEPTION | eff)

-- | Merge a list of signals from a channel into a single signal that expects a
-- | list.
createFeed :: forall action.
              Channel (List action) ->
              List (Signal action) ->
              Signal (List action)
createFeed chan signals = fromJust $ mergeMany $ reverse $
  (subscribe chan) : map (map singleton) signals

-- | Launch the affect and pipe the resulting list of actions into the given
-- | channel.
mapAffect :: forall action eff. (Action action) =>
             Channel (List action) ->
             AsyncAction action eff ->
             Eff (CoreEffects eff) Unit
mapAffect chan affect = launchAff $ do
  actions <- later affect
  liftEff $ send chan actions

-- | Initialize state management
initState :: forall action state eff. (Action action) =>
             Config action state eff ->
             Eff (CoreEffects eff) (Signal state)
initState config = do
  -- | Set up channels for actions and async effects
  actionChannel <- channel Nil
  affectChannel <- channel Nil
  let foldState state action = config.update action state
      foldActions actions state = foldl foldState state actions
      actionSignal = createFeed actionChannel $ fromFoldable config.actions
      stateSignal = foldp foldActions config.initialState actionSignal
      affectSignal = subscribe affectChannel
      effectSignal = affectSignal ~> map (mapAffect actionChannel)
  runSignal $ effectSignal ~> sequence_
  pure $ stateSignal
