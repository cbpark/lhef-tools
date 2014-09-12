{-# LANGUAGE RecordWildCards #-}

module HEP.Data.LHEF
    (
      module HEP.Data.LHEF.Type
    , module HEP.Data.LHEF.Parser
    , module HV

    , energyOf
    , idOf
    , is
    , finalStates
    , initialStates
    , getDaughters
    , particlesFrom
    )
    where

import           Control.Monad              (liftM)
import           Control.Monad.Trans.Reader
import qualified Data.IntMap                as M

import           HEP.Data.LHEF.Parser
import           HEP.Data.LHEF.Type
import           HEP.Vector                 as HV (HasFourMomentum (..))

energyOf :: Particle -> Double
energyOf Particle { pup = (_, _, _, e, _) } = e

idOf :: Particle -> Int
idOf Particle { .. } = idup

is :: Particle -> ParticleType -> Bool
p `is` pid = (`elem` getParType pid) . abs . idup $ p

initialStates :: Reader EventEntry [Particle]
initialStates = liftM M.elems $
                asks (M.filter (\Particle { .. } -> mothup == (1, 2)))

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
