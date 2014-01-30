{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (when)
import           System.Environment (getArgs)
import           System.Exit
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Attoparsec.ByteString.Lazy (parse, Result(..))
import           Data.List (partition)
import           Data.Double.Conversion.ByteString (toFixed)

import           HEP.Data.LHEF
import           HEP.Data.LHEF.Parser (parseEvent)
import qualified HEP.Vector.FourVector as FV

main :: IO ()
main = do
  args <- getArgs

  when (length args /= 1) $ do
         putStrLn "Usage: parsetest filename"
         exitFailure

  C.readFile (head args) >>= parseAndCalc . stripLHEF

stripLHEF :: C.ByteString -> C.ByteString
stripLHEF = C.unlines . init . dropWhile (/= C.pack "<event>") . C.lines

parseAndCalc :: C.ByteString -> IO ()
parseAndCalc str =
    case parse parseEvent str of
      Fail r _ _               -> C.putStr r
      Done evRemained evParsed -> do
        B.putStrLn $ invMass . fst . groupFinalStates $ snd evParsed
        parseAndCalc evRemained

groupFinalStates :: ParticleMap -> ([Particle], [Particle])
groupFinalStates pm = partition
                      (\p -> last (mothers pm (Just p))
                             == oldestMother1) visibleParticles
    where oldestMother1    = (head . initialStates) pm
          visibleParticles = filter visible $ finalStates pm

invMass :: [Particle] -> B.ByteString
invMass ps = toFixed 3 $
             FV.invariantMass $ foldr (FV.add . fourMomentum) FV.null4 ps

visible :: Particle -> Bool
visible (Particle {idup = pid}) = abs pid `elem` [5, 11, 13, 15]
