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

data Field = Field {
      fieldName :: String
    , fieldType :: String
    , fieldAttrs :: [Attribute]
    } deriving (Show)

data Method = Method {
      methodName :: String
    , methodType :: String
    , methodAttrs :: [Attribute]
    , invocations :: [Invocation]
    } deriving (Show)

data Invocation = Invocation {
      targetClass :: String
    , targetMethod :: String
    } deriving (Show)

type ConstantPool = Map Int CPEntry

data Attribute = Attribute {
      attrName :: String
    , attrBytes :: L.ByteString
    } deriving (Show)

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
parse bs = let (count, rem1) = getNum16 $ skipHeader bs
               (cp, rem2) = readConstantPool (count-1) rem1
               (fqn, rem3) = readClassname cp $ skipAccessFlags rem2
               (superclass, rem4) = readClassname cp rem3
               (interfaces, rem5) = readInterfaces cp rem4
               (fields, rem6) = readFields cp rem5
               (methods, rem7) = readMethods cp rem6
           in Class fqn superclass interfaces fields methods

-- FIXME see readUtf8
readClassname :: ConstantPool -> L.ByteString -> (String, L.ByteString)
readClassname cp bs = let (classIdx, rem) = getNum16 bs
                          Utf8 fqn = let Classref fqnIdx = cp ! classIdx
                                     in cp ! fqnIdx
                      in (fqn, rem)

readUtf8 :: ConstantPool -> L.ByteString -> (String, L.ByteString)
readUtf8 cp bs = let (idx, rem) = getNum16 bs
                     Utf8 s = cp ! idx
                 in (s, rem)

skipHeader :: L.ByteString -> L.ByteString
skipHeader = L8.drop 8
skipAccessFlags = L8.drop 2

readAttributes :: ConstantPool -> L.ByteString -> ([Attribute], L.ByteString)
readAttributes cp bs = uncurry readAttribute $ getNum16 bs
    where readAttribute 0 rem = ([], rem)
          readAttribute n rem = let (name, rem') = readUtf8 cp bs
                                    (length, rem'') = getNum32 bs
                                    len = fromIntegral length
                                    (attrs, rem''') = readAttribute (n-1) rem''
                                    (attr, rem'''') = mkAttr name len rem'''
                                in (attr : attrs, rem'''')

--mkAttr :: String -> Int -> L.ByteString -> (Attribute, L.ByteString)
mkAttr name len bs = (Attribute name (L.take len bs), L.drop len bs)

-- FIXME notice similarity with 'readFields', 'readMethods', 'readAttributes' and 'readConstantPoolEntries'
readInterfaces :: ConstantPool -> L.ByteString -> ([String], L.ByteString)
readInterfaces cp bs = uncurry readInterface $ getNum16 bs
    where readInterface 0 rem = ([], rem)
          readInterface n rem = let (fqn, rem') = readClassname cp rem
                                    (xs, rem'') = readInterface (n-1) rem'
                                in (fqn : xs, rem'')

readFields :: ConstantPool -> L.ByteString -> ([Field], L.ByteString)
readFields cp bs = uncurry readField $ getNum16 bs
    where readField 0 rem = ([], rem)
          readField n rem = let (name, rem') = readUtf8 cp $ skipAccessFlags rem
                                (t, rem'') = readUtf8 cp rem'
                                (attrs, rem''') = readAttributes cp rem''
                                (fs, rem'''') = readField (n-1) rem'''
                            in (Field name t attrs : fs, rem'''')

readMethods :: ConstantPool -> L.ByteString -> ([Method], L.ByteString)
readMethods cp bs = uncurry readMethod $ getNum16 bs
    where readMethod 0 rem = ([], rem)
          readMethod n rem = let (name, rem') = readUtf8 cp $ skipAccessFlags rem
                                 (t, rem'') = readUtf8 cp rem'
                                 (attrs, rem''') = readAttributes cp rem''
                                 (ms, rem'''') = readMethod (n-1) rem'''
                             in (Method name t attrs [] : ms, rem'''')

readConstantPool :: Int -> L.ByteString -> (ConstantPool, L.ByteString)
readConstantPool n bs = let (entries, rem) = readConstantPoolEntries n bs
                        in (Map.fromList $ [1..] `zip` entries, rem)

readConstantPoolEntries :: Int -> L.ByteString -> ([CPEntry], L.ByteString)
readConstantPoolEntries 0 bs = ([], bs)
readConstantPoolEntries n bs = let (e, rem) = readConstantPoolEntry bs
                                   (es, rem') = readConstantPoolEntries (n-1) rem
                               in (e : es, rem')

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
                                3  -> e1 Other (getNum32 $ snd tag)
                                4  -> e1 Other (getNum32 $ snd tag)
                                5  -> e2 Other2 (getNum32 $ snd tag) getNum32
                                6  -> e2 Other2 (getNum32 $ snd tag) getNum32
                                1  -> e1 Utf8 (getUtf8 $ snd tag)

getNum8 :: L.ByteString -> (Int, L.ByteString)
getNum8 bs = (fromIntegral $ L.head bs, L.tail bs)

-- FIXME use shift operator
getNum16 :: L.ByteString -> (Int, L.ByteString)
getNum16 bs = case L.unpack bs of
                x1:x2:rest -> ((fromIntegral x1) * 256 + fromIntegral x2, L.drop 2 bs)

-- FIXME use shift operator, revert
getNum32 :: L.ByteString -> (Int, L.ByteString)
getNum32 bs = case L.unpack bs of
                x1:x2:x3:x4:rest -> ((fromIntegral x1) * 16777216 + (fromIntegral x2) * 65536 + (fromIntegral x3) * 256 + fromIntegral x4, L.drop 4 bs)

getUtf8 :: L.ByteString -> (String, L.ByteString)
getUtf8 bs = let (length, rem) = getNum16 bs
                 n = fromIntegral length
             in (U8.toString $ L.take n rem, L.drop n rem)

-- FIXME remove, just to test stuff
main = test
test = do
  inh <- openBinaryFile "Test.class" ReadMode
  clazz <- L.hGetContents inh >>= \bs -> return (parse bs)
  putStrLn $ show clazz
  hClose inh
