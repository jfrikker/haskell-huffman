{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}

import Huffman.Code
import Text.PrettyPrint.Boxes
import qualified Data.Map as M
import qualified System.IO as IO
import qualified System.Environment as Environ
import qualified System.Console.CmdLib as CL
import System.Console.CmdLib ((%>))

data TableOptions = TableOptions {
  input :: String
} deriving (Show, Eq, CL.Typeable, CL.Data)

instance CL.Attributes TableOptions where
  attributes _ = CL.group "Options" [
    input %> [ CL.Positional 0, CL.ArgHelp "file", CL.Default "-" ]
    ]

instance CL.RecordCommand TableOptions where
  mode_summary _ = "Print the contents of a huffman coding file."

  run' opts _ = do
    inFile <- openInFile $ input opts
    treeS <- IO.hGetContents inFile
    let tree = read treeS :: Code Char
    let mapping = M.map show $ toMapping tree
    printBox $ mapToTable mapping

openInFile :: String -> IO IO.Handle
openInFile "-" = return IO.stdin
openInFile path = IO.openFile path IO.ReadMode

mapToTable :: M.Map String String -> Box
mapToTable m = column (M.keys m) <+> column (M.elems m)
  where column = foldl1 (//) . map text

main :: IO ()
main = do
  args <- Environ.getArgs
  let cmd = CL.recordCommand TableOptions {}
  CL.execute cmd args
