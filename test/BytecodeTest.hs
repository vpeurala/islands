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


parseJavaClassTest = TestCase (do clazz <- parseFile "test/JavaClass.class" 
                                  assertEqual "fqn" (fqn clazz) "test/JavaClass"
                                  assertEqual "super" (superclass clazz) "java/lang/Object"
                                  assertEqual "intfs" (interfaces clazz) ["java/lang/Comparable"]
                              )

parseJavaInterfaceTest = TestCase (do clazz <- parseFile "test/JavaInterface.class" 
                                      assertEqual "fqn" (fqn clazz) "test/JavaInterface"
                                      assertEqual "super" (superclass clazz) "java/lang/Object"
                                      assertEqual "intfs" (interfaces clazz) [])

parseFile name = do contents <- L.readFile name
                    return (parse contents)