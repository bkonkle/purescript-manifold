module Test.Manifold (testManifold) where

import Prelude (($), bind)

import Data.List (singleton)
import Data.Maybe (Maybe(..))
import Signal.Channel (send)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

import Manifold (createStore)

data Action = SetName String

type State = { name :: Maybe String }

initialState :: State
initialState = { name: Nothing }

update :: Action -> State -> State
update (SetName name) state = state { name = Just name }
update _ state = state

testManifold = do
  describe "Manifold" do
    describe "createStore" do
      it "reacts to actions using the update function" do
        let storeEffect = createStore update initialState
            actions = singleton $ SetName "Manifold"
            expected = { name: Just "Manifold" } :: State
        store <- storeEffect
        result <- store.state
        send store.channels.actions actions
        state <- result
        state `shouldEqual` expected
