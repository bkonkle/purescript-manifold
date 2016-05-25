module Test.Manifold (testManifold) where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Generic (class Generic, gEq, gShow)
import Data.List (fromFoldable, singleton)
import Data.Maybe (Maybe(Just, Nothing))
import Signal ((~>), runSignal)
import Signal.Channel (CHANNEL, send)
import Test.Unit (TestSuite, test, suite)
import Test.Unit.Assert (equal)

import Manifold (Update, runStore)

data Action = SetName String | ToggleActive

newtype State = State
  { name :: Maybe String
  , active :: Boolean }

derive instance genericState :: Generic State

instance showState :: Show State where
  show = gShow

instance eqState :: Eq State where
  eq = gEq

update :: Update Action State
update (SetName name) (State state) = State $ state { name = Just name }
update ToggleActive (State state) = State $ state { active = not state.active }

testManifold :: forall eff. TestSuite ( channel :: CHANNEL, err :: EXCEPTION | eff )
testManifold = suite "Manifold" do
  suite "runStore" do
    let initialState = State { name: Nothing, active: false }

    test "start a store with an initial state" $ liftEff do
      let expected = State { name: Nothing, active: false }
      store <- runStore update initialState
      runSignal $ store.stateSignal ~> equal expected ~> launchAff

    test "send an action and update the state" $ liftEff do
      let expected = State { name: Just "Candy Land", active: false }
      store <- runStore update initialState
      send store.actionChannel $ singleton (SetName "Candy Land")
      runSignal $ store.stateSignal ~> equal expected ~> launchAff

    test "send multiple actions" $ liftEff do
      let expected = State { name: Just "Cloud Cuckoo Land", active: true }
      store <- runStore update initialState
      send store.actionChannel $ fromFoldable [ ToggleActive, (SetName "Cloud Cuckoo Land") ]
      runSignal $ store.stateSignal ~> equal expected ~> launchAff
