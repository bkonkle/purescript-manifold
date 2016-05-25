module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Signal.Channel (CHANNEL)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

import Test.Manifold (testManifold)

main :: forall eff. Eff
        ( console :: CONSOLE
        , testOutput :: TESTOUTPUT
        , channel :: CHANNEL
        , err :: EXCEPTION
        | eff ) Unit
main = runTest do
  testManifold
