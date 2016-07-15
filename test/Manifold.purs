module Test.Manifold (testManifold) where

import Prelude
import Control.Monad.Aff (later)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Data.Array ((:))
import Signal.Channel (CHANNEL, send)
import Test.Unit (TIMER, TestSuite, test, suite)

import Manifold (runStore)
import Test.Utils (expect)
import Test.Examples.Todos.State.Todos (State(..), Action(..), Todo(..), update)

testManifold :: forall eff. TestSuite
  ( channel :: CHANNEL
  , err :: EXCEPTION
  , avar :: AVAR
  , timer :: TIMER
  , ref :: REF | eff )
testManifold = suite "Manifold" do
  suite "runStore" do

    test "start a store with an initial state" do
      let initialState = State { todos: [], lastId: 0 }
      store <- liftEff $ runStore update initialState
      expect 1 store.stateSignal [initialState]

    test "send an action and update the state" do
      let initialState = State { todos: [], lastId: 0 }
          todo = Todo { text: "Play Candy Land", id: 1, completed: false }
          expected = State { todos: [ todo ], lastId: 1 }
      store <- liftEff $ runStore update initialState
      liftEff $ send store.actionChannel $ [ Add "Play Candy Land" ]
      expect 1 store.stateSignal [expected]

    test "send multiple actions" do
      let initialState = State { todos: [], lastId: 0 }
          todo = Todo { text: "Play Candy Land", id: 1, completed: true }
          expected = State { todos: [ todo ], lastId: 1 }
      store <- liftEff $ runStore update initialState
      liftEff $ send store.actionChannel $ [ Add "Play Candy Land", Complete 1 true ]
      expect 1 store.stateSignal [expected]

    test "send an effect that yields an action" do
      let initialState = State { todos: [], lastId: 0 }
          effect = pure $ [ Add "Play Candy Land" ]
          todo = Todo { text: "Play Candy Land", id: 1, completed: false }
          expected = State { todos: [ todo ], lastId: 1 }
      store <- liftEff $ runStore update initialState
      liftEff $ send store.effectChannel effect
      later $ expect 1 store.stateSignal [expected]

    test "send an effect that yields multiple actions" do
      let initialState = State { todos: [], lastId: 0 }
          effect = pure $ [ Add "Play Candy Land", Complete 1 true ]
          todo = Todo { text: "Play Candy Land", id: 1, completed: true }
          expected = State { todos: [ todo ], lastId: 1 }
      store <- liftEff $ runStore update initialState
      liftEff $ send store.effectChannel effect
      later $ expect 1 store.stateSignal [expected]
