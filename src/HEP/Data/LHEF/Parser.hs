{-# LANGUAGE OverloadedStrings #-}

module HEP.Data.LHEF.Parser
    (
      parseEvent
    , stripLHEF
    ) where

import           Control.Applicative  ((<*))
import           Data.Attoparsec.Text
import           Data.IntMap          (fromList)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           HEP.Data.LHEF

parseEventInfo :: Parser EventInfo
parseEventInfo = do skipSpace
                    nup'    <- signed decimal
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
                    return EventInfo { nup    = nup'
                                     , idprup = idprup'
                                     , xwgtup = xwgtup'
                                     , scalup = scalup'
                                     , aqedup = aqedup'
                                     , aqcdup = aqcdup' }

parseParticle :: Parser Particle
parseParticle = do skipSpace
                   idup'    <- signed decimal
                   skipSpace
                   istup'   <- signed decimal
                   skipSpace
                   mothup1' <- signed decimal
                   skipSpace
                   mothup2' <- signed decimal
                   skipSpace
                   icolup1' <- signed decimal
                   skipSpace
                   icolup2' <- signed decimal
                   skipSpace
                   pup1'    <- double
                   skipSpace
                   pup2'    <- double
                   skipSpace
                   pup3'    <- double
                   skipSpace
                   pup4'    <- double
                   skipSpace
                   pup5'    <- double
                   skipSpace
                   vtimup'  <- double
                   skipSpace
                   spinup'  <- double
                   return Particle { idup   = idup'
                                   , istup  = istup'
                                   , mothup = (mothup1', mothup2')
                                   , icolup = (icolup1', icolup2')
                                   , pup    = (pup1', pup2', pup3', pup4', pup5')
                                   , vtimup = vtimup'
                                   , spinup = spinup' }

parseParticleEntries :: Parser [Particle]
parseParticleEntries = many1' $ parseParticle <* endOfLine

parseOpEvInfo :: Parser [()]
parseOpEvInfo =  many' $ char '#' >> skipWhile (not . isEndOfLine) <* endOfLine

parseEvent :: Parser Event
parseEvent = do skipSpace
                string "<event>"
                evInfo <- parseEventInfo <* endOfLine
                parEntries <- parseParticleEntries
                parseOpEvInfo
                string "</event>"
                endOfLine
                return (evInfo, fromList $ zip [1..] parEntries)

stripLHEF :: Text -> Text
stripLHEF = T.unlines . init . dropWhile (/="<event>") . T.lines
