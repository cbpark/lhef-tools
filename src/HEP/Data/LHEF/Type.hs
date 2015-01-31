module HEP.Data.LHEF.Type where

import           Data.IntMap                         (IntMap)
import           HEP.Kinematics                      (HasFourMomentum (..))
import           HEP.Kinematics.Vector.LorentzVector (setXYZT)

data EventInfo = EventInfo
    { nup    :: Int    -- ^ Number of particle entries in the event.
    , idprup :: Int    -- ^ ID of the process for the event.
    , xwgtup :: Double -- ^ Event weight.
    , scalup :: Double -- ^ Scale of the event in GeV.
    , aqedup :: Double -- ^ The QED coupling \alpha_{QED} used for the event.
    , aqcdup :: Double -- ^ The QCD coupling \alpha_{QCD} used for the event.
    } deriving Show

data Particle = Particle
    { -- | Particle ID according to Particle Data Group convention.
      idup   :: !Int
      -- | Status code.
    , istup  :: !Int
      -- | Index of first and last mother.
    , mothup :: !(Int, Int)
      -- | Integer tag for the color flow line passing through the
      -- (anti-)color of the particle.
    , icolup :: (Int, Int)
      -- | Lab frame momentum (P_x, P_y, P_z, E, M) of particle in GeV.
    , pup    :: !(Double, Double, Double, Double, Double)
      -- | Invariant lifetime (distance from production to decay) in mm.
    , vtimup :: Double
      -- | Consine of the angle between the spin-vector of particle and
      -- the three-momentum of the decaying particle, specified in the
      -- lab frame.
    , spinup :: Double
    } deriving (Show)

instance HasFourMomentum Particle where
  fourMomentum (Particle { pup = (x, y, z, e, _) }) = setXYZT x y z e
  pt (Particle { pup = (x, y, _, _, _) }) = sqrt $! x ** 2 + y ** 2

type EventEntry = IntMap Particle
type Event = (EventInfo, EventEntry)
newtype ParticleType = ParticleType { getParType :: [Int] } deriving (Show, Eq)
