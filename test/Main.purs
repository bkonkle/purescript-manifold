module Test.Main where

import Prelude (Unit)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Random (RANDOM())
import Node.Process (PROCESS())
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

-- main :: forall e. Eff (process :: PROCESS, console :: CONSOLE, random :: RANDOM | e) Unit
-- main = run [consoleReporter] do
