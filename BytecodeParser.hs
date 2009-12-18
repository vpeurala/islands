import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import System.IO -- FIXME can be removed 


type Method = String

data CallGraph = CallGraph {
      method :: Method
    , callsites :: [CallGraph]
    } deriving (Show)

-- http://www.murrayc.com/learning/java/java_classfileformat.shtml
parse :: L.ByteString -> [CallGraph]
parse = []

skipHeader :: L.ByteString -> L.ByteString
skipHeader = L8.drop 8


-- FIXME remove, just for testing stuff
main = test
test = do
  inh <- openBinaryFile "Test.class" ReadMode
  graph <- L.hGetContents inh >>= \bs -> return (parse bs)
  putStrLn $ show graph
  hClose inh
