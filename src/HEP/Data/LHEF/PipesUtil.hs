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
         getLHEFEvent
       , eventEntry
       , eventEntryFromHandle
       , eventEntryFromBS
       , initialStates
       , finalStates
       , groupByMother
       ) where

import           Control.Monad                    (forever)
import           Control.Monad.Trans.State.Strict
import           Data.ByteString.Char8            (ByteString)
import           Data.Function                    (on)
import qualified Data.IntMap                      as M
import           Data.List                        (groupBy)
import           Pipes
import           Pipes.Attoparsec                 (parse)
import           Pipes.ByteString                 (fromHandle)
import qualified Pipes.Prelude                    as P
import           System.IO                        (Handle)

import           HEP.Data.LHEF.Parser             (lhefEvent)
import           HEP.Data.LHEF.Type

getLHEFEvent :: Monad m => Producer ByteString m () -> Producer Event m ()
getLHEFEvent s = do (r, s') <- lift $ runStateT (parse lhefEvent) s
                    case r of Just (Right ev) -> yield ev >> getLHEFEvent s'
                              _               -> return ()

eventEntry :: MonadIO m => (a -> Producer ByteString m ())
            -> a -> Producer EventEntry m ()
eventEntry f = eventEntry' . f
  where eventEntry' s = getLHEFEvent s >-> P.map snd

eventEntryFromBS :: MonadIO m => ByteString -> Producer EventEntry m ()
eventEntryFromBS = eventEntry yield

eventEntryFromHandle :: MonadIO m => Handle -> Producer EventEntry m ()
eventEntryFromHandle = eventEntry fromHandle

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
