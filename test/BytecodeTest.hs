module Test.Islands.Bytecode where

import Test.HUnit
import System.IO
import qualified Data.ByteString.Lazy as L
import Islands.Bytecode

-- Run tests manually (until we have a proper build file):
-- :l src/Opcodes.hs src/BytecodeParser.hs test/BytecodeTest.hs
-- :m Test.HUnit Test.Islands.Bytecode
-- runTestTT tests

tests = TestList [ TestLabel "Parses Java class" parseJavaClassTest
                 , TestLabel "Parses Java interface" parseJavaInterfaceTest ]


-- FIXME assert
parseJavaClassTest = TestCase (do inh <- openBinaryFile "test/Test.class" ReadMode
                                  clazz <- L.hGetContents inh >>= return . parse
                                  putStrLn $ show clazz
                                  hClose inh)

-- FIXME actual test
parseJavaInterfaceTest = TestCase (assertEqual "test" (1,2) (1,2))


