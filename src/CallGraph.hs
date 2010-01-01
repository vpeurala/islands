module Islands.Dependencies where

import Islands.Bytecode (Class, Method)
import qualified Islands.Bytecode as B
import Data.Map (Map, (!), lookup)
import qualified Data.Map as Map
import Control.Monad (join)

data CallGraph = CallGraph {
      method :: FQMethod
    , callsites :: [CallGraph]
    } deriving (Show)

data FQMethod = FQMethod {
      declaringClass :: String
    , methodName :: String
    , methodSignature :: String
    } deriving (Show, Eq, Ord)

mkGraph :: [Class] -> FQMethod -> CallGraph
mkGraph classes root = let methods = methodMap classes
                           xxx r = CallGraph r (map xxx $ next r)
                           next method = map fromInvocation $ maybe [] B.invocations (Map.lookup method methods)
                       in xxx root

methodMap :: [Class] -> Map FQMethod Method
methodMap classes = let allMethods = join $ map methods classes
                    in Map.fromList $ zip (qualify allMethods) (map snd allMethods)
                        where methods c = map (\m -> (c, m)) (B.methods c)
                              qualify = map (uncurry sig)

sig clazz method = FQMethod (B.fqn clazz) (B.methodName method) (B.methodType method)

-- FIXME unify Invocation & FQMethod
fromInvocation i = FQMethod (B.targetClass i) (B.targetMethodName i) (B.targetMethodSignature i)
