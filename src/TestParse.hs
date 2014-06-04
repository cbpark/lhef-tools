module Main where

import           HEP.Data.LHEF.Parser            (parseEvent, stripLHEF)

import           Control.Monad                   (when)
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import qualified Data.ByteString.Lazy.Char8      as C
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure)

parseAndPrint :: C.ByteString -> IO ()
parseAndPrint str = case parse parseEvent str of
                      Fail r _ _               -> C.putStr r
                      Done evRemained evParsed -> do
                                                print (snd evParsed)
                                                parseAndPrint evRemained

main :: IO ()
main = do
  args <- getArgs

  when (length args /= 1) $ do
         putStrLn "Usage: testlhefparse filename"
         exitFailure

  evstr <- C.readFile (head args)
  (parseAndPrint . stripLHEF) evstr
