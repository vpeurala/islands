import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.UTF8 as U8
import qualified Data.ByteString.Lazy as L
import Data.Map (Map, (!))
import qualified Data.Map as Map
import System.IO -- FIXME can be removed 
import Debug.Trace -- FIXME can be removed 

data Class = Class {
      fqn :: String
    , superclass :: String
    , interfaces :: [String]
    , fields :: [Field]
    , methods :: [Method]
    } deriving (Show)

type Field = String

data Method = Method {
      name :: String
    , invocations :: [Invocation]
    } deriving (Show)

data Invocation = Invocation {
      classFqn :: String
    , method :: String
    } deriving (Show)

type ConstantPool = Map Int CPEntry

type NameIdx = Int
type ClassIdx = Int
type NameAndTypeIdx = Int
type DescriptorIdx = Int
type Utf8Idx = Int
data CPEntry = Classref NameIdx
             | Fieldref ClassIdx NameAndTypeIdx
             | Methodref ClassIdx NameAndTypeIdx 
             | InterfaceMethodref ClassIdx NameAndTypeIdx
             | NameAndType NameIdx DescriptorIdx
             | Stringref Utf8Idx
             | Utf8 String
             | Other Int -- stuff in constant pool which does not interest us (values etc.)
             | Other2 Int Int
               deriving (Show)

-- http://www.murrayc.com/learning/java/java_classfileformat.shtml
-- FIXME clean up manual threading of rems
parse :: L.ByteString -> Class
parse bs = let (count, rem1) = readCount16 $ skipHeader bs
               (cp, rem2) = readConstantPool (count-1) rem1
               (classIdx, rem3) = getNum16 $ skipAccessFlags rem2
               (superclassIdx, rem4) = getNum16 rem3
               fqn = parseClassname cp classIdx
               superclass = parseClassname cp superclassIdx
           in Class fqn superclass [] [] []

readClassname :: ConstantPool -> L.ByteString -> (String, L.ByteString)
readClassname cp bs = let (classIdx, rem) = getNum16 bs
                          Utf8 fqn = let Classref fqnIdx = cp ! classIdx
                                     in cp ! fqnIdx
                      in (fqn, rem)

parseClassname :: ConstantPool -> Int -> String
parseClassname cp classIdx = let Utf8 fqn = let Classref fqnIdx = cp ! classIdx
                                            in cp ! fqnIdx
                             in fqn

skipHeader :: L.ByteString -> L.ByteString
skipHeader = L8.drop 8
skipAccessFlags = L8.drop 2

-- FIXME notice similarity with 'readConstantPoolEntries'
readInterfaces :: ConstantPool -> L.ByteString -> ([String], L.ByteString)
readInterfaces cp bs = let (count, rem1) = readCount16 bs
                       in readInterface count rem1
                           where readInterface 0 rem = ([], rem)
                                 readInterface n rem = let (fqn, rem') = readClassname cp rem
                                                           (xs, rem'') = readInterface (n-1) rem'
                                                       in (fqn : xs, rem'')

-- FIXME remove
readCount16 :: L.ByteString -> (Int, L.ByteString)
readCount16 = getNum16

readConstantPool :: Int -> L.ByteString -> (ConstantPool, L.ByteString)
readConstantPool n bs = let (entries, rem) = readConstantPoolEntries n bs
                        in (Map.fromList $ [1..] `zip` entries, rem)

readConstantPoolEntries :: Int -> L.ByteString -> ([CPEntry], L.ByteString)
readConstantPoolEntries 0 bs = ([], bs)
readConstantPoolEntries n bs = let (e, rem1) = readConstantPoolEntry bs
                                   (es, rem2) = readConstantPoolEntries (n-1) rem1
                               in (e : es, rem2)

-- FIXME cleanup this ugly implementation, how to chain (x, rem) ?
readConstantPoolEntry :: L.ByteString -> (CPEntry, L.ByteString)
readConstantPoolEntry bs = let tag = getNum8 bs
                               e1 entry (idx, bs) = (entry idx, bs)
                               e2 entry (idx1, bs) f = let (idx2, rem) = f bs
                                                        in (entry idx1 idx2, rem)
                           in case fst tag of
                                7  -> e1 Classref (getNum16 $ snd tag)
                                9  -> e2 Fieldref (getNum16 $ snd tag) getNum16
                                10 -> e2 Methodref (getNum16 $ snd tag) getNum16
                                11 -> e2 InterfaceMethodref (getNum16 $ snd tag) getNum16
                                12 -> e2 NameAndType (getNum16 $ snd tag) getNum16
                                8  -> e1 Stringref (getNum16 $ snd tag)
                                3  -> e1 Other (getInt $ snd tag)
                                4  -> e1 Other (getInt $ snd tag)
                                5  -> e2 Other2 (getInt $ snd tag) getInt
                                6  -> e2 Other2 (getInt $ snd tag) getInt
                                1  -> e1 Utf8 (getUtf8 $ snd tag)

getNum8 :: L.ByteString -> (Int, L.ByteString)
getNum8 bs = (fromIntegral $ L.head bs, L.tail bs)

getNum16 :: L.ByteString -> (Int, L.ByteString)
getNum16 bs = case L.unpack bs of
                x : y : rest -> ((fromIntegral x) * 256 + fromIntegral y, L.drop 2 bs)

getUtf8 :: L.ByteString -> (String, L.ByteString)
getUtf8 bs = let (length, rem) = getNum16 bs
                 n = fromIntegral length
             in (U8.toString $ L.take n rem, L.drop n rem)

getInt :: L.ByteString -> (Int, L.ByteString)
getInt = undefined

-- FIXME remove, just to test stuff
main = test
test = do
  inh <- openBinaryFile "Test.class" ReadMode
  clazz <- L.hGetContents inh >>= \bs -> return (parse bs)
  putStrLn $ show clazz
  hClose inh
