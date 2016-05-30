module Test.Manifold (testManifold) where

import Prelude

import Control.Monad.Aff (later)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe(Just, Nothing))
import Signal.Channel (CHANNEL, send)
import Test.Unit (TIMER, TestSuite, test, suite)
import Test.Utils (expect)

import Manifold (Update, runStore)

data Action = SetName String | ToggleActive

newtype State = State { name :: Maybe String, active :: Boolean }

derive instance genericState :: Generic State

instance showState :: Show State where
  show = gShow

instance eqState :: Eq State where
  eq = gEq

update :: Update Action State
update (SetName name) (State state) = State $ state { name = Just name }
update ToggleActive (State state) = State $ state { active = not state.active }

testManifold :: forall eff. TestSuite
  ( channel :: CHANNEL
  , err :: EXCEPTION
  , avar :: AVAR
  , timer :: TIMER
  , ref :: REF | eff )
testManifold = suite "Manifold" do
  suite "runStore" do
    let initialState = State { name: Nothing, active: false }

    test "start a store with an initial state" do
      store <- liftEff $ runStore update initialState
      expect 1 store.stateSignal [initialState]

    test "send an action and update the state" do
      let expected = State { name: Just "Candy Land", active: false }
      store <- liftEff $ runStore update initialState
      liftEff $ send store.actionChannel $ [ SetName "Candy Land" ]
      expect 1 store.stateSignal [expected]

    test "send multiple actions" do
      let expected = State { name: Just "Cloud Cuckoo Land", active: true }
      store <- liftEff $ runStore update initialState
      liftEff $ send store.actionChannel $ [ ToggleActive, SetName "Cloud Cuckoo Land" ]
      expect 1 store.stateSignal [expected]

    test "send an effect that yields an action" do
      let expected = State { name: Just "Land of the Lost", active: false }
          effect = pure $ [ SetName "Land of the Lost" ]
      store <- liftEff $ runStore update initialState
      liftEff $ send store.effectChannel effect
      later $ expect 1 store.stateSignal [expected]

    test "send an effect that yields multiple actions" do
      let expected = State { name: Just "Land O'Lakes", active: true }
          effect = pure $ [ ToggleActive, SetName "Land O'Lakes" ]
      store <- liftEff $ runStore update initialState
      liftEff $ send store.effectChannel effect
      later $ expect 1 store.stateSignal [expected]
