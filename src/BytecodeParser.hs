module Islands.Bytecode where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.UTF8 as U8
import qualified Data.ByteString.Lazy as L
import List (find)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Word (Word8)
import Bits (shift)
import Control.Monad (join, replicateM, liftM)
import qualified Islands.Bytecode.Opcodes as Op
import Debug.Trace

data Class = Class {
      fqn :: String
    , superclass :: String
    , interfaces :: [String]
    , fields :: [Field]
    , methods :: [Method]
    } deriving (Show, Eq)

data Field = Field {
      fieldName :: String
    , fieldType :: String
    , fieldAttrs :: [Attribute]
    } deriving (Show, Eq)

data Method = Method {
      methodName :: String
    , methodType :: String
    , methodAttrs :: [Attribute]
    , invocations :: [Invocation]
    } deriving (Show, Eq)

data Invocation = Invocation {
      targetClass :: String
    , targetMethodName :: String
    , targetMethodSignature :: String
    } deriving (Show, Eq)

type ConstantPool = Map Int CPEntry

data Attribute = Attribute {
      attrName :: String
    , attrBytes :: L.ByteString  -- FIXME remove
    } deriving (Show, Eq)

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

-- FIXME cleanup all fromIntegral conversions
parse :: L.ByteString -> Class
parse bs = unbox parse0 bs

parse0 :: Parse Class
parse0 = do header <- skipN 8
            count <- getNum16
            cp <- readConstantPool (count-1)
            flags <- skipAccessFlags
            fqn <- readClassname cp
            superclass <- readClassname cp
            interfaces <- readInterfaces cp
            fields <- readFields cp
            methods <- readMethods cp
            return (Class fqn superclass interfaces fields methods)

readClassname :: ConstantPool -> Parse String
readClassname cp = do classIdx <- getNum16
                      let Utf8 fqn = let Classref fqnIdx = cp ! classIdx
                                     in cp ! fqnIdx
                      return fqn

skipAccessFlags :: Parse ()
skipAccessFlags = skipN 2

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
                                 code <- getN codeLen
                                 exps <- skipExceptionTable
                                 attrs <- skipCodeAttributes
                                 return (Attribute name code)
                    _ -> Parse(\s -> (Attribute name (L.take len s), L.drop len s))

skipExceptionTable :: Parse[()]
skipExceptionTable = do count <- getNum16
                        replicateM count (skipN 8)

skipCodeAttributes :: Parse [()]
skipCodeAttributes = do count <- getNum16
                        replicateM count skipCodeAttr
                            where skipCodeAttr = do nameIdx <- getNum16
                                                    len <- getNum32
                                                    skipN len

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
    where findCodeAttr = find (\a -> attrName a == "Code")
          code attrs = maybe L.empty attrBytes (findCodeAttr attrs)
          invocations code = invocation code
              where invocation c | L.length c == 0 = []
                    invocation c = (resolveInvocation cp $ map fromIntegral (L.unpack c)) : (invocation (L.drop (fromIntegral $ Op.length (fromIntegral $ L.head c)) c))

resolveInvocation :: ConstantPool -> [Int] -> [Invocation]
resolveInvocation cp (c:x:y:t) | or $ map (==c) Op.invokeInstructions = 
                                   let Methodref classIdx nameAndTypeIdx = cp ! toNum16 x y
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
readConstantPoolEntries count = repeatx count
    where repeatx :: Int -> Parse [CPEntry]
          repeatx n | n <= 0 = return []
          repeatx n = let e = readConstantPoolEntry 
                      in concat `liftM` (sequence $ (sequence [e]) : (restOfEntries n e) : [])
                          where restOfEntries :: Int -> Parse CPEntry -> Parse [CPEntry]
                                restOfEntries n' e = join $ (rec n') `liftM` e
                                rec :: Int -> CPEntry -> Parse [CPEntry]
                                rec n' pureE = repeatx $ entriesLeft pureE ((trace $ "> " ++ show n') n')

entriesLeft :: CPEntry -> Int -> Int
entriesLeft pureE n = case pureE of
                        (Other2 _ _) -> n-2
                        _ -> n-1

readConstantPoolEntry :: Parse CPEntry
readConstantPoolEntry = do tag <- getNum8
                           mkEntry ((trace $ show tag) tag)

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

readUtf8 :: ConstantPool -> Parse String
readUtf8 cp = do idx <- getNum16
                 let Utf8 s = cp ! idx
                 return s

getNum8 :: Parse Int
getNum8 = Parse(\bs -> (fromIntegral $ L.head bs, L.tail bs))

getNum16 :: Parse Int
getNum16 = Parse(\bs -> case L.unpack bs of
                          x:y:rest -> (toNum16 (fromIntegral x) (fromIntegral y), L.drop 2 bs))

toNum16 x y = x `shift` 8 + y

getNum32 :: Parse Int
getNum32 = do high <- getNum16
              low <- getNum16
              return (high `shift` 16 + low)

getUtf8 :: Parse String
getUtf8 = do length <- getNum16
             let n = fromIntegral length
             Parse(\s -> (U8.toString $ L.take n s, L.drop n s))

getN :: Int -> Parse L.ByteString
getN n = Parse(\s -> (L.take (fromIntegral n) s, L.drop (fromIntegral n) s))

skipN :: Int -> Parse ()
skipN n = Parse(\s -> ((), L.drop (fromIntegral n) s))
