{-# LANGUAGE OverloadedStrings #-}

module HEP.Data.LHEF.Parser
    (
      lhefEvent
    , lhefEvents
    , stripLHEF
    ) where

import           Data.Attoparsec.ByteString       (skipWhile)
import           Data.Attoparsec.ByteString.Char8 hiding (skipWhile)
import           Data.ByteString.Lazy.Char8       (ByteString)
import qualified Data.ByteString.Lazy.Char8       as C
import           Data.IntMap                      (fromList)

import           HEP.Data.LHEF.Type

eventInfo :: Parser EventInfo
eventInfo = do skipSpace
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

particle :: Parser Particle
particle = do skipSpace
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

lhefEvent :: Parser Event
lhefEvent = do skipSpace
               manyTill' skipTillEnd (string "<event>" >> endOfLine)
               evInfo <- eventInfo <* endOfLine
               parEntries <- many1' $ particle <* endOfLine
               opEvInfo
               string "</event>" >> endOfLine
               finalLine
               return (evInfo, fromList $ zip [1..] parEntries)
  where opEvInfo = many' $ char '#' >> skipTillEnd
        finalLine = many' $ string "</LesHouchesEvents>" >> endOfLine
        skipTillEnd = skipWhile (not . isEndOfLine) >> endOfLine

lhefEvents :: Parser [Event]
lhefEvents = string "<LesHouchesEvents version=" >> many1' lhefEvent

stripLHEF :: ByteString -> ByteString
stripLHEF = C.unlines . init . dropWhile (/="<event>") . C.lines
