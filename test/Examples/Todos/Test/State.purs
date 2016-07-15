module Test.Examples.Todos.Test.State (todosExampleTestState) where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Data.Array ((:))
import Test.Unit (TIMER, TestSuite, suite, test)
import Test.Unit.Assert (equal)

import Test.Examples.Todos.State.Todos (Action(..), State(..), Todo(..), update)

todosExampleTestState :: forall eff. TestSuite
  ( err :: EXCEPTION, avar :: AVAR, timer :: TIMER, ref :: REF | eff )
todosExampleTestState = suite "Examples.Todos.State" do
  suite "Todos" do

    suite "update" do

      test "handles Add actions" do
        let initialState = State
              { todos: [ Todo { text: "Use Redux", id: 0, completed: false } ]
              , lastId: 0 }
            todo = Todo { text: "Become cool", id: 1, completed: false }
            expected = initialState #
              \(State state) -> State
                state { todos = todo : state.todos, lastId = 1 }
            result = update (Add "Become cool") initialState
        result `equal` expected

      test "handles Delete actions" do
        let initialState = State
              { todos: [ Todo { text: "Use Redux", id: 0, completed: false } ]
              , lastId: 0 }
            expected = State { todos: [], lastId: 0 }
            result = update (Delete 0) initialState
        result `equal` expected

      test "handles Edit actions" do
        let initialState = State
              { todos: [ Todo { text: "Use Redux", id: 0, completed: false } ]
              , lastId: 0 }
            expected = State
              { todos: [ Todo { text: "Use PureScript", id: 0, completed: false } ]
              , lastId: 0 }
            result = update (Edit 0 "Use PureScript") initialState
        result `equal` expected

      test "handles Complete actions" do
        let initialState = State
              { todos: [ Todo { text: "Use Redux", id: 0, completed: false } ]
              , lastId: 0 }
            expected = State
              { todos: [ Todo { text: "Use Redux", id: 0, completed: true } ]
              , lastId: 0 }
            result = update (Complete 0 true) initialState
        result `equal` expected

      test "handles CompleteAll actions" do
        let initialState = State
              { todos:
                [ Todo { text: "Use Redux", id: 0, completed: false }
                , Todo { text: "With PureScript", id: 0, completed: false } ]
              , lastId: 0 }
            expected = State
              { todos:
                [ Todo { text: "Use Redux", id: 0, completed: true }
                , Todo { text: "With PureScript", id: 0, completed: true } ]
              , lastId: 0 }
            result = update CompleteAll initialState
        result `equal` expected

      test "handles ClearComplete actions" do
        let initialState = State
              { todos:
                [ Todo { text: "Use Redux", id: 0, completed: true }
                , Todo { text: "With PureScript", id: 0, completed: true } ]
              , lastId: 0 }
            expected = State { todos: [], lastId: 0 }
            result = update ClearComplete initialState
        result `equal` expected
