{-# LANGUAGE OverloadedStrings #-}

module HEP.Data.LHEF.Parser
    (
      lhefEvent
    , lhefEvents
    , stripLHEF
    ) where

import           Control.Applicative              ((<*))
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
               return EventInfo { _nup    = nup'
                                , _idprup = idprup'
                                , _xwgtup = xwgtup'
                                , _scalup = scalup'
                                , _aqedup = aqedup'
                                , _aqcdup = aqcdup' }

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
              return Particle { _idup   = idup'
                              , _istup  = istup'
                              , _mothup = (mothup1', mothup2')
                              , _icolup = (icolup1', icolup2')
                              , _pup    = (pup1', pup2', pup3', pup4', pup5')
                              , _vtimup = vtimup'
                              , _spinup = spinup' }

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
