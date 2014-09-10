{-# LANGUAGE TemplateHaskell #-}

module HEP.Data.LHEF.Type where

import           Control.Lens
import           Data.IntMap  (IntMap)

data EventInfo = EventInfo
    { -- | Number of particle entries in the event.
      _nup    :: Int
      -- | ID of the process for the event.
    , _idprup :: Int
      -- | Event weight.
    , _xwgtup :: Double
      -- | Scale of the event in GeV.
    , _scalup :: Double
      -- | The QED coupling \alpha_{QED} used for the event.
    , _aqedup :: Double
      -- | The QCD coupling \alpha_{QCD} used for the event.
    , _aqcdup :: Double
    } deriving Show

makeLenses ''EventInfo

data Particle = Particle
    { -- | Particle ID according to Particle Data Group convention.
      _idup   :: Int
      -- | Status code.
    , _istup  :: Int
      -- | Index of first and last mother.
    , _mothup :: (Int, Int)
      -- | Integer tag for the color flow line passing through the
      -- (anti-)color of the particle.
    , _icolup :: (Int, Int)
      -- | Lab frame momentum (P_x, P_y, P_z, E, M) of particle in GeV.
    , _pup    :: (Double, Double, Double, Double, Double)
      -- | Invariant lifetime (distance from production to decay) in mm.
    , _vtimup :: Double
      -- | Consine of the angle between the spin-vector of particle and
      -- the three-momentum of the decaying particle, specified in the
      -- lab frame.
    , _spinup :: Double
    } deriving (Eq, Show)

makeLenses ''Particle

type EventEntry = IntMap Particle
type Event = (EventInfo, EventEntry)
newtype ParticleType = ParticleType { getParType :: [Int] }
