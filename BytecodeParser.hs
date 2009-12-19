import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import System.IO -- FIXME can be removed 
import Debug.Trace -- FIXME can be removed 


type Method = String

--data CallGraph = CallGraph {
--      method :: Method
--    , callsites :: [CallGraph]
--    } deriving (Show)

data Invocations = Invocations {
      method :: Method
    , callsites :: [Method]
    } deriving (Show)

-- http://www.murrayc.com/learning/java/java_classfileformat.shtml
parse :: L.ByteString -> [Invocations]
parse bs = []

foo bs = constantPoolCount $ skipHeader bs

skipHeader :: L.ByteString -> L.ByteString
skipHeader = L8.drop 8

constantPoolCount :: L.ByteString -> (Int, L.ByteString)
constantPoolCount bs = getNum2 bs

getNum2 :: L.ByteString -> (Int, L.ByteString)
getNum2 bs = case L.unpack bs of
              x : y : rest -> ((fromIntegral x) * 16 + fromIntegral y, L.drop 2 bs)


-- FIXME remove, just for testing stuff
main = test
test = do
  inh <- openBinaryFile "Test.class" ReadMode
  graph <- L.hGetContents inh >>= \bs -> return (parse bs)
  putStrLn $ show graph
  x <- L.hGetContents inh >>= \bs -> return (foo bs)
  putStrLn $ show x
  hClose inh
