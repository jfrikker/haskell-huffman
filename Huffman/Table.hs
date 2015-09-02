import Huffman.Code
import Huffman.Frequency as Freq
import Data.Maybe
import Text.PrettyPrint.Boxes
import qualified Data.Map as M
import qualified System.IO as IO

mapToTable :: M.Map String String -> Box
mapToTable m = column (M.keys m) <+> column (M.elems m)
  where column = foldl1 (//) . map text

main :: IO ()
main = do
  freqs <- Freq.fromStream IO.stdin
  let code = fromJust $ buildCode freqs
  let mapping = M.map show $ toMapping code
  printBox $ mapToTable mapping
