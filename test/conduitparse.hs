module Main where

import           Control.Monad              (when)
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Conduit
import           Data.Conduit.Attoparsec    (conduitParser)
import qualified Data.Conduit.Binary        as CB
import qualified Data.Conduit.List          as CL
import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)
import           System.IO

import           HEP.Data.LHEF.Parser

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
         putStrLn "Usage: testlhefparse filename"
         exitFailure

  let infile = head args
  putStrLn $ "-- Parsing " ++ show infile ++ "."
  withFile infile ReadMode $ \inh -> do
    evstr <- C.hGetContents inh
    CB.sourceLbs evstr $$ conduitParser lhefEvent =$ CL.mapM_ (print . snd)
