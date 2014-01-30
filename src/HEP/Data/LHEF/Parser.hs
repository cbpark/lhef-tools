module HEP.Data.LHEF.Parser
    ( parseEventInfo
    , parseEvent
    , parseAllEvents
    ) where

import Data.Attoparsec.ByteString.Char8 (Parser, skipSpace, signed, string,
                                         char, decimal, double, many1,
                                         endOfLine)
import Data.ByteString.Char8 (pack)
import Control.Applicative ((<*), (<|>))
import Data.Map (fromList)

import HEP.Data.LHEF

parseEventInfo :: Parser EventInfo
parseEventInfo = do
  skipSpace
  string $ pack "<init>"
  skipSpace
  nup' <- signed decimal
  skipSpace
  idprup' <- signed decimal
  skipSpace
  xwgtup' <- double
  skipSpace
  scalup' <- double
  skipSpace
  aqedup' <- double
  skipSpace
  aqcdup' <- double
  skipSpace
  string $ pack "</init>"

  return EventInfo { nup = nup'
                   , idprup = idprup'
                   , xwgtup = xwgtup'
                   , scalup = scalup'
                   , aqedup = aqedup'
                   , aqcdup = aqcdup'
                   }

parseParticle :: Parser Particle
parseParticle = do
  skipSpace
  idup' <- signed decimal
  skipSpace
  istup' <- signed decimal
  skipSpace
  mothup1' <- signed decimal
  skipSpace
  mothup2' <- signed decimal
  skipSpace
  icolup1' <- signed decimal
  skipSpace
  icolup2' <- signed decimal
  skipSpace
  pup1' <- double
  skipSpace
  pup2' <- double
  skipSpace
  pup3' <- double
  skipSpace
  pup4' <- double
  skipSpace
  pup5' <- double
  skipSpace
  vtimup' <- doubleWithTrailingDot
  skipSpace
  spinup' <- doubleWithTrailingDot

  return Particle { idup   = idup'
                  , istup  = istup'
                  , mothup = (mothup1', mothup2')
                  , icolup = (icolup1', icolup2')
                  , pup    = (pup1', pup2', pup3', pup4', pup5')
                  , vtimup = vtimup'
                  , spinup = spinup'
                  }

parseParticleEntries :: Parser [Particle]
parseParticleEntries = many1 $ parseParticle <* endOfLine

parseEvent :: Parser Event
parseEvent = do
  skipSpace
  string $ pack "<event>"
  evInfo <- parseEventInfo
  parEntries <- parseParticleEntries
  string $ pack "</event>"

  let parMap = fromList $ zip [1..] parEntries
  return (evInfo, parMap)

parseAllEvents :: Parser [Event]
parseAllEvents = many1 $ parseEvent <* endOfLine

-- | Parse a double number with a trailing dot (ex. 3.).
doubleWithTrailingDot :: Parser Double
doubleWithTrailingDot = do
  d <- double
  char '.' >> return () <|> return ()    -- Remove the trailing dot.
  return d
