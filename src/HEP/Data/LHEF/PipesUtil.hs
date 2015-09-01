--------------------------------------------------------------------------------
-- |
-- Module      :  HEP.Data.LHEF.PipesUtil
-- Copyright   :  (c) 2015 Chan Beom Park
-- License     :  BSD-style
-- Maintainer  :  Chan Beom Park <cbpark@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Helper functions for analyses of LHEF data files using pipes.
--
--------------------------------------------------------------------------------

module HEP.Data.LHEF.PipesUtil
       (
         initialStates
       , finalStates
       , groupByMother
       ) where

import           Control.Monad      (forever)
import           Data.Function      (on)
import qualified Data.IntMap        as M
import           Data.List          (groupBy)
import           Pipes
import qualified Pipes.Prelude      as P

import           HEP.Data.LHEF.Type

getParticles :: Monad m => (Particle -> Bool) -> Pipe EventEntry [Particle] m ()
getParticles f = forever $ particles >-> getSome
  where particles = P.map M.elems
        getSome = void $ await >>= yield . filter f

initialStates :: Monad m => Pipe EventEntry [Particle] m ()
initialStates = getParticles ((==1) . fst . mothup)

finalStates :: Monad m => Pipe EventEntry [Particle] m ()
finalStates = getParticles ((==1) . istup)

groupByMother :: Monad m => Pipe [Particle] [[Particle]] m ()
groupByMother = P.map (groupBy ((==) `on` mothup))
