module Test.Manifold (testManifold) where

import Prelude (($), Unit, bind)

import Control.Monad.Eff.Random (RANDOM)
import Data.List (singleton)
import Data.Maybe (Maybe(..))
import Signal.Channel (send)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Manifold (createStore)

data Action = SetName String

type State = { name :: Maybe String }

initialState :: State
initialState = { name: Nothing }

update :: Action -> State -> State
update (SetName name) state = state { name = Just name }
update _ state = state

testManifold :: forall r. Spec (random :: RANDOM | r ) Unit
testManifold = do
  describe "Manifold" do
    describe "createStore" do
      it "reacts to actions using the update function" do
        let storeEffect = createStore update initialState
            actions = singleton $ SetName "Manifold"
            expected :: State
            expected = { name: Just "Manifold" }
        store <- storeEffect
        result <- store.state
        send store.channels.actions actions
        state <- result
        state `shouldEqual` expected
