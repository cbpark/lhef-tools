{-# LANGUAGE RecordWildCards #-}

module HEP.Data.LHEF
    (
      module HEP.Data.LHEF.Type
    , module HEP.Data.LHEF.Parser

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

import           Control.Monad              (liftM)
import           Control.Monad.Trans.Reader
import           Data.Function              (on)
import qualified Data.IntMap                as M
import           Data.List                  (nub)

import           HEP.Vector.LorentzVector

import           HEP.Data.LHEF.Parser
import           HEP.Data.LHEF.Type

fourMomentum :: Particle -> LorentzVector Double
fourMomentum Particle { pup = (x, y, z, e, _) } = lorentzVectorXYZT (x, y, z, e)

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

initialStates :: EventEntry -> [Particle]
initialStates pm = nub $ map (ancenstor pm) (runReader finalStates pm)
    where ancenstor pm' = last . familyLine pm' . Just

familyLine :: EventEntry -> Maybe Particle -> [Particle]
familyLine _  Nothing  = []
familyLine pm (Just p) = p : familyLine pm (mother pm p)
    where mother pm' Particle { mothup = (m, _) }
              | m `elem` [1,2] = Nothing
              | otherwise      = M.lookup m pm'

finalStates :: Reader EventEntry [Particle]
finalStates = liftM M.elems $ asks (M.filter (\Particle { .. } -> istup == 1))

particlesFrom :: ParticleType -> Reader EventEntry [[Particle]]
particlesFrom pid = asks (M.keys . M.filter (`is` pid)) >>= mapM getDaughters

getDaughters :: Int -> Reader EventEntry [Particle]
getDaughters i = do
  pm <- ask
  daughters <- asks $ M.filter (\Particle { .. } -> fst mothup == i)
  return $ M.foldrWithKey
             (\k p acc -> case istup p of
                            1 -> p : acc
                            _ -> runReader (getDaughters k) pm ++ acc) []
             daughters
