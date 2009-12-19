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
             | Other -- there's stuff in constant pool which does not interest us (values etc.)

-- http://www.murrayc.com/learning/java/java_classfileformat.shtml
parse :: L.ByteString -> Class
parse bs = Class "com.example.Foo" []

skipHeader :: L.ByteString -> L.ByteString
skipHeader = L8.drop 8

readConstantPoolCount :: L.ByteString -> (Int, L.ByteString)
readConstantPoolCount bs = getNum2 bs

readConstantPoolEntries :: Int -> L.ByteString -> ([CPEntry], L.ByteString)
readConstantPoolEntries 0 bs = ([], bs)
readConstantPoolEntries x bs = let (e, rem1) = readConstantPoolEntry bs
                                   (es, rem2) = readConstantPoolEntries (x-1) rem1
                               in (e : es, rem2)

-- FIXME cleanup this ugly implementation, how to chain (x, rem) ?
readConstantPoolEntry :: L.ByteString -> (CPEntry, L.ByteString)
readConstantPoolEntry bs = let tag = getNum1 bs
                               e1 entry (idx, bs) = (entry idx, bs)
                               e2 entry (idx1, bs) f = let (idx2, rem) = f bs
                                                        in (entry idx1 idx2, rem)
                           in case fst tag of
                                7  -> e1 Classref (getNum2 $ snd tag)
                                9  -> e2 Fieldref (getNum2 $ snd tag) getNum2
                                10 -> e2 Methodref (getNum2 $ snd tag) getNum2
                                11 -> e2 InterfaceMethodref (getNum2 $ snd tag) getNum2
                                12 -> e2 NameAndType (getNum2 $ snd tag) getNum2
                                _  -> (Other, snd tag)

getNum1 :: L.ByteString -> (Int, L.ByteString)
getNum1 bs = (fromIntegral $ L.head bs, L.tail bs)

getNum2 :: L.ByteString -> (Int, L.ByteString)
getNum2 bs = case L.unpack bs of
              x : y : rest -> ((fromIntegral x) * 16 + fromIntegral y, L.drop 2 bs)


-- FIXME remove, just to test stuff
foo bs = readConstantPoolCount $ skipHeader bs
main = test
test = do
  inh <- openBinaryFile "Test.class" ReadMode
  graph <- L.hGetContents inh >>= \bs -> return (parse bs)
  putStrLn $ show graph
  x <- L.hGetContents inh >>= \bs -> return (foo bs)
  putStrLn $ show x
  hClose inh
