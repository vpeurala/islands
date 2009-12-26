module Islands.Bytecode where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.UTF8 as U8
import qualified Data.ByteString.Lazy as L
import List (find)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Word (Word8)
import Control.Monad (join, replicateM)
import qualified Islands.Bytecode.Opcodes as Op
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
    , targetMethodName :: String
    , targetMethodSignature :: String
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

newtype Parse a = Parse(L.ByteString -> (a, L.ByteString))

unbox :: Parse a -> L.ByteString -> a
unbox (Parse p) bs = fst $ p bs

identity :: a -> Parse a
identity a = Parse(\s -> (a, s))

(==>) :: Parse a -> (a -> Parse b) -> Parse b
(Parse p1) ==> p2 = Parse(\s -> let (x, newState) = p1 s 
                                    (Parse q) = p2 x
                                in q newState)

instance Monad Parse where
    return = identity
    (>>=) = (==>)

-- FIXME cleanup all fromIntegral conversions (perhaps by using Word8?)
parse :: L.ByteString -> Class
parse bs = unbox parse0 bs

parse0 :: Parse Class
parse0 = do header <- skipHeader
            count <- getNum16
            cp <- readConstantPool (count-1)
            flags <- skipAccessFlags
            fqn <- readClassname cp
            superclass <- readClassname cp
            interfaces <- readInterfaces cp
            fields <- readFields cp
            methods <- readMethods cp
            return (Class fqn superclass interfaces fields methods)

-- FIXME see readUtf8
readClassname :: ConstantPool -> Parse String
readClassname cp = do classIdx <- getNum16
                      let Utf8 fqn = let Classref fqnIdx = cp ! classIdx
                                     in cp ! fqnIdx
                      return fqn

readUtf8 :: ConstantPool -> Parse String
readUtf8 cp = do idx <- getNum16
                 let Utf8 s = cp ! idx
                 return s

skipHeader :: Parse ()
skipHeader = Parse(\s -> ((), L8.drop 8 s))

skipAccessFlags :: Parse ()
skipAccessFlags = Parse(\s -> ((), L8.drop 2 s))

readAttributes :: ConstantPool -> Parse [Attribute]
readAttributes cp = do count <- getNum16
                       replicateM count readAttribute
                           where readAttribute = do name <- readUtf8 cp
                                                    length <- getNum32
                                                    mkAttr name (fromIntegral length)

--mkAttr :: String -> Int -> Parse Attribute
mkAttr name len = case name of
                    "Code" -> do maxStack <- getNum16
                                 maxLocals <- getNum16
                                 codeLen <- getNum32
                                 code <- Parse(\s -> (L.take (fromIntegral codeLen) s, L.drop (fromIntegral codeLen) s))
                                 exps <- skipExceptionTable
                                 attrs <- skipCodeAttributes
                                 return (Attribute name code)
                    _ -> Parse(\s -> (Attribute name (L.take len s), L.drop len s))

skipExceptionTable :: Parse[()]
skipExceptionTable = do count <- getNum16
                        replicateM count skipEntry
                            where skipEntry = Parse(\s -> ((), L8.drop 8 s))

skipCodeAttributes :: Parse [()]
skipCodeAttributes = do count <- getNum16
                        replicateM count skipCodeAttr
                            where skipCodeAttr = do nameIdx <- getNum16
                                                    len <- getNum32
                                                    Parse(\s -> ((), L.drop (fromIntegral len) s))

readInterfaces :: ConstantPool -> Parse [String]
readInterfaces cp = do count <- getNum16
                       replicateM count (readClassname cp)

readFields :: ConstantPool -> Parse [Field]
readFields cp = do count <- getNum16
                   replicateM count (readField cp)

readField cp = do flags <- skipAccessFlags
                  name <- readUtf8 cp
                  signature <- readUtf8 cp
                  attrs <- readAttributes cp
                  return (Field name signature attrs)

readMethods :: ConstantPool -> Parse [Method]
readMethods cp = do count <- getNum16
                    replicateM count readMethod
                        where readMethod = do flags <- skipAccessFlags
                                              name <- readUtf8 cp
                                              signature <- readUtf8 cp
                                              attrs <- readAttributes cp
                                              return (mkMethod cp name signature attrs)

mkMethod :: ConstantPool -> String -> String -> [Attribute] -> Method
mkMethod cp name sig attrs = Method name sig attrs $ join (invocations (code attrs))
    where code attrs = maybe L.empty attrBytes (findCodeAttr attrs)
          invocations code = invocation code
              where invocation c | L.length c == 0 = []
                    invocation c = (resolveInvocation cp $ map fromIntegral (L.unpack c)) : (invocation (L.drop (fromIntegral $ Op.length (fromIntegral $ L.head c)) c))

findCodeAttr :: [Attribute] -> Maybe Attribute
findCodeAttr = find (\a -> attrName a == "Code")

resolveInvocation :: ConstantPool -> [Int] -> [Invocation]
resolveInvocation cp (c:x:y:t) | or $ map (==c) Op.invokeInstructions = 
                                   let Methodref classIdx nameAndTypeIdx = cp ! ((x*256)+y)
                                       Classref cnameIdx = cp ! classIdx
                                       Utf8 classname = cp ! cnameIdx
                                       NameAndType mnameIdx sigIdx = cp ! nameAndTypeIdx
                                       Utf8 methodname = cp ! mnameIdx
                                       Utf8 signature = cp ! sigIdx
                                   in [Invocation classname methodname signature]
resolveInvocation _ _ = []

readConstantPool :: Int -> Parse ConstantPool
readConstantPool n = do entries <- readConstantPoolEntries n 
                        return (Map.fromList $ [1..] `zip` entries)

readConstantPoolEntries :: Int -> Parse [CPEntry]
readConstantPoolEntries n = replicateM n readConstantPoolEntry 

readConstantPoolEntry :: Parse CPEntry
readConstantPoolEntry = do tag <- getNum8
                           mkEntry tag

mkEntry :: Int -> Parse CPEntry
mkEntry tag = case tag of
                7  -> do idx <- getNum16; return (Classref idx)
                9  -> do cidx <- getNum16; nidx <- getNum16; return (Fieldref cidx nidx)
                10 -> do cidx <- getNum16; nidx <- getNum16; return (Methodref cidx nidx)
                11 -> do cidx <- getNum16; nidx <- getNum16; return (InterfaceMethodref cidx nidx)
                12 -> do nidx <- getNum16; didx <- getNum16; return (NameAndType nidx didx)
                8  -> do idx <- getNum16; return (Stringref idx)
                3  -> do idx <- getNum32; return (Other idx)
                4  -> do idx <- getNum32; return (Other idx)
                5  -> do idx1 <- getNum32; idx2 <- getNum32; return (Other2 idx1 idx2)
                6  -> do idx1 <- getNum32; idx2 <- getNum32; return (Other2 idx1 idx2)
                1  -> do idx <- getUtf8; return (Utf8 idx)

getNum8 :: Parse Int
getNum8 = Parse(\bs -> (fromIntegral $ L.head bs, L.tail bs))

-- FIXME use shift operator
getNum16 :: Parse Int
getNum16 = Parse(\bs -> case L.unpack bs of
                          x1:x2:rest -> ((fromIntegral x1) * 256 + fromIntegral x2, L.drop 2 bs))

-- FIXME use shift operator
getNum32 :: Parse Int
getNum32 = do high <- getNum16
              low <- getNum16
              return (high * 65536 + low)

getUtf8 :: Parse String
getUtf8 = do length <- getNum16
             let n = fromIntegral length
             Parse(\s -> (U8.toString $ L.take n s, L.drop n s))

-- FIXME remove, just to test stuff
main = test
test = do
  inh <- openBinaryFile "Test.class" ReadMode
  clazz <- L.hGetContents inh >>= return . parse
  putStrLn $ show clazz
  hClose inh
