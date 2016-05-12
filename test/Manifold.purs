module Test.Manifold where

import Control.Comonad.Store (Store)
import Data.List (List, singleton)
import Data.Maybe (Maybe(..))
import Manifold (CoreEffects, createStore)
import Prelude (unit, ($), return, bind)
import Signal (runSignal)
import Signal.Channel (send)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

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
        let actions = singleton $ SetName "Manifold"
            storeEffect = createStore update initialState
            expected = { name: Just "Manifold" } :: State
        store <- storeEffect
        return unit
