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

import           Control.Lens
import           Control.Monad              (liftM)
import           Control.Monad.Trans.Reader
import           Data.Function              (on)
import qualified Data.IntMap                as M
import           Data.List                  (nub)

import           HEP.Vector.LorentzVector

import           HEP.Data.LHEF.Parser
import           HEP.Data.LHEF.Type

fourMomentum :: Particle -> LorentzVector Double
fourMomentum p = lorentzVectorXYZT (p^.pup._1, p^.pup._2, p^.pup._3, p^.pup._4)

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
energyOf p = p^.pup._4

idOf :: Particle -> Int
idOf = view idup

is :: Particle -> ParticleType -> Bool
p `is` pid = (`elem` getParType pid) . abs . (^.idup) $ p

initialStates :: EventEntry -> [Particle]
initialStates pm = nub $ map (ancenstor pm) (runReader finalStates pm)
    where ancenstor pm' = last . familyLine pm' . Just

familyLine :: EventEntry -> Maybe Particle -> [Particle]
familyLine _  Nothing  = []
familyLine pm (Just p) = p : familyLine pm (mother pm p)
  where mother pm' p' | p'^.mothup._1 `elem` [1,2] = Nothing
                      | otherwise                  = M.lookup (p'^.mothup._1) pm'

finalStates :: Reader EventEntry [Particle]
finalStates = liftM M.elems $ asks (M.filter ((==1) . (^.istup)))

particlesFrom :: ParticleType -> Reader EventEntry [[Particle]]
particlesFrom pid = asks (M.keys . M.filter (`is` pid)) >>= mapM getDaughters

getDaughters :: Int -> Reader EventEntry [Particle]
getDaughters i = do
  pm <- ask
  daughters <- asks $ M.filter ((==i) . (^.mothup._1))
  return $ M.foldrWithKey
             (\k p acc -> case p^.istup of
                            1 -> p : acc
                            _ -> runReader (getDaughters k) pm ++ acc) []
             daughters
