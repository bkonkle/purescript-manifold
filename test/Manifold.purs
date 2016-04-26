module Test.Manifold where

import Prelude (($), Unit)

import Control.Monad.Eff.Random (RANDOM())
import Data.List (List(Nil))
import Data.Maybe (Maybe(..))
import Signal (Signal)
import Signal.Channel (channel)
import Test.Spec (Spec(), describe, it)
import Test.Spec.Assertions

import Manifold (createFeed)

-- testManifold :: forall r. Spec (random :: RANDOM | r ) Unit
-- testManifold = do
--   describe "Manifold" do
--     describe "createFeed" do
--       it "merges a list of signals into signal for a reverse order list of values" do
--         let chan = channel Nil
--             signals =
