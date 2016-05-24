module Test.Main where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Generic (class Generic, gEq, gShow)
import Data.List (singleton)
import Data.Maybe (Maybe(Just, Nothing))
import Manifold (createStore)
import Signal ((~>), runSignal)
import Signal.Channel (CHANNEL, send)
import Test.Unit (test, suite)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

data Action = SetName String

newtype State = State { name :: Maybe String }

derive instance genericState :: Generic State

instance showState :: Show State where
  show = gShow

instance eqState :: Eq State where
  eq = gEq

main :: forall eff. Eff
  ( console :: CONSOLE
  , testOutput :: TESTOUTPUT
  , channel :: CHANNEL
  , err :: EXCEPTION
  | eff ) Unit
main = runTest do
  suite "Manifold" do
    suite "createStore" do
      let initialState = State { name: Nothing }
          update (SetName name) (State state) = State $ state { name = Just name }
      test "create a store with an initial state" $ liftEff do
        let expected = State { name: Nothing }
        store <- createStore update initialState
        runSignal $ store.stateSignal ~> equal expected ~> launchAff
      test "send an action and update the state" $ liftEff do
        let expected = State { name: Just "Candy Land" }
        store <- createStore update initialState
        send store.actionChannel $ singleton (SetName "Candy Land")
        runSignal $ store.stateSignal ~> equal expected ~> launchAff
