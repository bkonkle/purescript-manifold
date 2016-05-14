module Main where

import Manifold
import Data.Maybe (Maybe)
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

type State = { name :: Maybe String }



-- main = run [consoleReporter] do
--   describe "Manifold" do
--     it "does something, I'm sure of it!" do
--
