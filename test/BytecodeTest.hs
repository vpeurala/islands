module Test.Islands.Bytecode where

import Test.HUnit
import System.IO
import qualified Data.ByteString.Lazy as L
import List (find)
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
                                  assertEqual "invocations" (getInvocations clazz "foo")
                                              [ Invocation "test/JavaClass$Inner" "<init>" "(Ltest/JavaClass;)V"
                                              , Invocation "test/JavaClass$Inner" "bar" "()Ljava/lang/String;" 
                                              , Invocation "java/io/PrintStream" "println" "(Ljava/lang/String;)V" ]
                              )

parseJavaInterfaceTest = TestCase (do clazz <- parseFile "test/JavaInterface.class" 
                                      assertEqual "fqn" (fqn clazz) "test/JavaInterface"
                                      assertEqual "super" (superclass clazz) "java/lang/Object"
                                      assertEqual "intfs" (interfaces clazz) []
                                      assertEqual "invocations" (getInvocations clazz "foo") [])

parseFile name = do contents <- L.readFile name
                    return (parse contents)

getInvocations clazz name = maybe [] invocations findMethod
    where findMethod = find (\m -> methodName m == name) (methods clazz)
