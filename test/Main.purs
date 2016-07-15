module Test.Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Signal.Channel (CHANNEL)
import Test.Unit (TIMER)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

import Test.Examples.Todos.Test.State (todosExampleTestState)
import Test.Manifold (testManifold)

main :: forall eff. Eff
        ( console :: CONSOLE
        , testOutput :: TESTOUTPUT
        , channel :: CHANNEL
        , err :: EXCEPTION
        , avar :: AVAR
        , timer :: TIMER
        , ref :: REF
        | eff ) Unit
main = runTest do
  testManifold
  todosExampleTestState
