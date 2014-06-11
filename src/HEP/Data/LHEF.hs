{-# LANGUAGE RecordWildCards #-}

module HEP.Data.LHEF
    (
      Event
    , EventInfo (..)
    , Particle (..)
    , ParticleMap

    , energyOf
    , fourMomentum
    , finalStates
    , familyLine
    , getDaughters
    , idOf
    , initialStates
    , inParticles
    , particleLineOf
    , particlesFrom
    , threeMomentum
    )
    where

import           HEP.Vector.LorentzVector (LorentzVector (..))
import           HEP.Vector.ThreeVector   (ThreeVector (..))

import qualified Data.IntMap              as IntMap
import           Data.List                (nub)

data EventInfo = EventInfo
    { -- | Number of particle entries in the event.
      nup    :: Int
      -- | ID of the process for the event.
    , idprup :: Int
      -- | Event weight.
    , xwgtup :: Double
      -- | Scale of the event in GeV.
    , scalup :: Double
      -- | The QED coupling \alpha_{QED} used for the event.
    , aqedup :: Double
      -- | The QCD coupling \alpha_{QCD} used for the event.
    , aqcdup :: Double
    } deriving Show

data Particle = Particle
    { -- | Particle ID according to Particle Data Group convention.
      idup   :: Int
      -- | Status code.
    , istup  :: Int
      -- | Index of first and last mother.
    , mothup :: (Int, Int)
      -- | Integer tag for the color flow line passing through the
      -- (anti-)color of the particle.
    , icolup :: (Int, Int)
      -- | Lab frame momentum (P_x, P_y, P_z, E, M) of particle in GeV.
    , pup    :: (Double, Double, Double, Double, Double)
      -- | Invariant lifetime (distance from production to decay) in mm.
    , vtimup :: Double
      -- | Consine of the angle between the spin-vector of particle and
      -- the three-momentum of the decaying particle, specified in the
      -- lab frame.
    , spinup :: Double
    } deriving Show

instance Eq Particle where
    p == p' = (idup p == idup p') && (pup p == pup p')

instance Ord Particle where
    p >= p' = pt p >= pt p'
        where pt Particle { pup = (x, y, _, _, _) } = sqrt $ x*x + y*y

type ParticleMap = IntMap.IntMap Particle

type Event = (EventInfo, ParticleMap)

fourMomentum :: Particle -> LorentzVector Double
fourMomentum Particle { pup = (x, y, z, e, _) } = LorentzVector e x y z

threeMomentum :: Particle -> ThreeVector Double
threeMomentum Particle { pup = (x, y, z, _, _) } = ThreeVector x y z

energyOf :: Particle -> Double
energyOf Particle { pup = (_, _, _, e, _) } = e

idOf :: Particle -> Int
idOf Particle { .. } = idup

inParticles :: [Int] -> Particle -> Bool
inParticles ns = (`elem` ns) . abs . idup

particlesFrom :: [Int] -> ParticleMap -> [[Particle]]
particlesFrom ns pm = let pl = particleLineOf ns pm
                      in map (getDaughters pm) pl

finalStates :: ParticleMap -> [Particle]
finalStates = IntMap.elems . IntMap.filter (\Particle { .. } -> istup == 1)

familyLine :: ParticleMap -> Maybe Particle -> [Particle]
familyLine _  Nothing  = []
familyLine pm (Just p) = p : familyLine pm (mother pm p)
    where mother pm' Particle { mothup = (m, _) }
              | m `elem` [1, 2] = Nothing
              | otherwise       = IntMap.lookup m pm'

initialStates :: ParticleMap -> [Particle]
initialStates pm = nub $ map (ancenstor pm) (finalStates pm)
    where ancenstor pm' = last . familyLine pm' . Just

getDaughters :: ParticleMap -> Int -> [Particle]
getDaughters pm i = map snd (getDaughters' pm i)

getDaughters' :: ParticleMap -> Int -> [(Int, Particle)]
getDaughters' pm i = stablePars (daughters i pm)
    where daughters i' = IntMap.toList .
                         IntMap.filter (\Particle { .. } -> fst mothup == i')

          stablePars []      = []
          stablePars ((n, p):ps)
              | istup p == 1 = (n, p) : stablePars ps
              | otherwise    = getDaughters' pm n ++ stablePars ps

particleLineOf :: [Int] -> ParticleMap -> [Int]
particleLineOf ns = IntMap.keys .
                    IntMap.filter (\Particle { .. } -> abs idup `elem` ns)
