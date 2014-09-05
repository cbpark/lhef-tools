{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad        (when)
import           Data.Attoparsec.Text
import           Data.Text            (Text)
import qualified Data.Text.IO         as TI
import           System.Environment   (getArgs)
import           System.Exit          (exitFailure)

import           HEP.Data.LHEF.Parser (parseEvent, stripLHEF)

parseAndPrint :: Text -> IO ()
parseAndPrint str =
  case parse parseEvent str of
   Fail r _ _               -> TI.putStrLn r
   Partial _                -> putStrLn "-- Done."
   Done evRemained evParsed -> do print (snd evParsed)
                                  parseAndPrint evRemained

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
         putStrLn "Usage: testlhefparse filename"
         exitFailure

  let infile = head args
  putStrLn $ "-- Parsing " ++ show infile ++ "."
  evstr <- TI.readFile infile
  (parseAndPrint . stripLHEF) evstr
