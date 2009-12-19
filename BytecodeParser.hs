import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import System.IO -- FIXME can be removed 
import Debug.Trace -- FIXME can be removed 

data Class = Class {
      fqn :: String
    , methods :: [Method]
    } deriving (Show)

data Method = Method {
      name :: String
    , invocations :: [Invocation]
    } deriving (Show)

data Invocation = Invocation {
      classFqn :: String
    , method :: String
    } deriving (Show)

type NameIdx = Int
type ClassIdx = Int
type NameAndTypeIdx = Int
type DescriptorIdx = Int
data CPEntry = Classref NameIdx
             | Fieldref ClassIdx NameAndTypeIdx
             | Methodref ClassIdx NameAndTypeIdx 
             | InterfaceMethodref ClassIdx NameAndTypeIdx
             | NameAndType NameIdx DescriptorIdx

-- http://www.murrayc.com/learning/java/java_classfileformat.shtml
parse :: L.ByteString -> Class
parse bs = Class "com.example.Foo" []

skipHeader :: L.ByteString -> L.ByteString
skipHeader = L8.drop 8

readConstantPoolCount :: L.ByteString -> (Int, L.ByteString)
readConstantPoolCount bs = getNum2 bs

readConstantPoolEntry :: L.ByteString -> (CPEntry, L.ByteString)
readConstantPoolEntry = undefined

getNum1 :: L.ByteString -> (Int, L.ByteString)
getNum1 bs = (fromIntegral $ L.head bs, L.tail bs)

getNum2 :: L.ByteString -> (Int, L.ByteString)
getNum2 bs = case L.unpack bs of
              x : y : rest -> ((fromIntegral x) * 16 + fromIntegral y, L.drop 2 bs)


-- FIXME remove, just for testing stuff
foo bs = readConstantPoolCount $ skipHeader bs
main = test
test = do
  inh <- openBinaryFile "Test.class" ReadMode
  graph <- L.hGetContents inh >>= \bs -> return (parse bs)
  putStrLn $ show graph
  x <- L.hGetContents inh >>= \bs -> return (foo bs)
  putStrLn $ show x
  hClose inh
