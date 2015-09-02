{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}

import Huffman.Code
import qualified Huffman.Frequency as Freq
import Data.Maybe
import qualified System.IO as IO
import qualified System.Environment as Environ
import qualified System.Console.CmdLib as CL
import System.Console.CmdLib ((%>))
import Control.Monad (foldM)

data GenOptions = GenOptions {
  out :: String
} deriving (Show, Eq, CL.Typeable, CL.Data)

instance CL.Attributes GenOptions where
  attributes _ = CL.group "Options" [
    out %> [ CL.Short "o", CL.Long ["out"], CL.Help "The file to output the coding to.", CL.ArgHelp "file", CL.Default "-" ]
    ]

instance CL.RecordCommand GenOptions where
  mode_summary _ = "Generate a Huffman coding from sample text."

  run' opts [] = CL.run' opts ["-"]
  run' opts inPaths = do
    outFile <- openOutFile $ out opts
    freqs <- foldM updateFreqs Freq.empty inPaths
    IO.hPrint outFile $ fromJust $ buildCode freqs
    IO.hClose outFile

openOutFile :: String -> IO IO.Handle
openOutFile "-" = return IO.stdout
openOutFile path = IO.openFile path IO.WriteMode

openInFile :: String -> IO IO.Handle
openInFile "-" = return IO.stdin
openInFile path = IO.openFile path IO.ReadMode

updateFreqs :: Freq.FrequencyTable Char -> String -> IO (Freq.FrequencyTable Char)
updateFreqs freq path = do
  inFile <- openInFile path
  freq' <- Freq.incrementFromStream inFile freq
  IO.hClose inFile
  return freq'

main :: IO ()
main = do
  args <- Environ.getArgs
  let cmd = CL.recordCommand GenOptions {}
  CL.execute cmd args
