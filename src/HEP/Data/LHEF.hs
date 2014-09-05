{-# LANGUAGE RecordWildCards #-}

module HEP.Data.LHEF
    (
      Event
    , EventInfo (..)
    , Particle (..)
    , ParticleType (..)
    , ParticleMap

    , cosTheta
    , dR
    , energyOf
    , fourMomentum
    , finalStates
    , getDaughters
    , idOf
    , initialStates
    , invMass
    , is
    , particlesFrom
    , rapidity
    , transMass
    , transMassOne
    , transMomentum
    , transMomentumOne
    )
    where

import           Control.Monad
import           Control.Monad.Trans.Reader
import           Data.Function              (on)
import           Data.IntMap                (IntMap)
import qualified Data.IntMap                as M
import           Data.List                  (nub)

import           HEP.Vector
import           HEP.Vector.LorentzVector

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
    } deriving (Eq, Show)

type ParticleMap = IntMap Particle
type Event = (EventInfo, ParticleMap)
newtype ParticleType = ParticleType { getParType :: [Int] }

fourMomentum :: Particle -> LorentzVector Double
fourMomentum Particle { pup = (x, y, z, e, _) } = LorentzVector e x y z

momentumSum :: [Particle] -> LorentzVector Double
momentumSum = vectorSum . map fourMomentum

invMass :: [Particle] -> Double
invMass = invariantMass . momentumSum

transMass :: [Particle] -> Particle -> Double
transMass ps k = transverseMass (momentumSum ps) (fourMomentum k)

transMassOne :: Particle -> Particle -> Double
transMassOne = transverseMass `on` fourMomentum

transMomentum :: [Particle] -> Double
transMomentum = pT . momentumSum

transMomentumOne :: Particle -> Double
transMomentumOne = pT . fourMomentum

rapidity :: Particle -> Double
rapidity = eta . fourMomentum

cosTheta :: [Particle] -> Maybe Double
cosTheta [p,p'] = Just . cos $ (deltaTheta `on` fourMomentum) p p'
cosTheta _      = Nothing

dR :: [Particle] -> Maybe Double
dR [p,p'] = Just $ (deltaR `on` fourMomentum) p p'
dR _      = Nothing

energyOf :: Particle -> Double
energyOf Particle { pup = (_, _, _, e, _) } = e

idOf :: Particle -> Int
idOf Particle { .. } = idup

is :: Particle -> ParticleType -> Bool
p `is` pid = (`elem` getParType pid) . abs . idup $ p

initialStates :: ParticleMap -> [Particle]
initialStates pm = nub $ map (ancenstor pm) (runReader finalStates pm)
    where ancenstor pm' = last . familyLine pm' . Just

familyLine :: ParticleMap -> Maybe Particle -> [Particle]
familyLine _  Nothing  = []
familyLine pm (Just p) = p : familyLine pm (mother pm p)
    where mother pm' Particle { mothup = (m, _) }
              | m `elem` [1,2] = Nothing
              | otherwise      = M.lookup m pm'

finalStates :: Reader ParticleMap [Particle]
finalStates = liftM M.elems $ asks (M.filter (\Particle { .. } -> istup == 1))

particlesFrom :: ParticleType -> Reader ParticleMap [[Particle]]
particlesFrom pid = asks (M.keys . M.filter (`is` pid)) >>= mapM getDaughters

getDaughters :: Int -> Reader ParticleMap [Particle]
getDaughters i = do
  pm <- ask
  daughters <- asks $ M.filter (\Particle { .. } -> fst mothup == i)
  return $ M.foldrWithKey
             (\k p acc -> case istup p of
                            1 -> p : acc
                            _ -> runReader (getDaughters k) pm ++ acc) []
             daughters
