module Islands.Dependencies where

import Islands.Bytecode (Class, Method)
import qualified Islands.Bytecode as B
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Control.Monad (join)

data CallGraph = CallGraph {
      method :: (Class, Method)
    , callsites :: [CallGraph]
    } deriving (Show)

data FQMethod = FQMethod {
      declaringClass :: String
    , methodName :: String
    , methodSignature :: String
    } deriving (Show, Eq, Ord)

mkGraph :: [Class] -> (Class, Method) -> CallGraph
mkGraph classes root = let methods = methodMap classes                                     
                       in xxx root
                           where xxx r = CallGraph r (map xxx (methods ! r))
                                 next method = B.invocations (methods ! method)

methodMap :: [Class] -> Map FQMethod Method
methodMap classes = let allMethods = join $ map methods classes
                    in Map.fromList $ zip (qualify allMethods) (map snd allMethods)
                        where methods c = map (\m -> (c, m)) (B.methods c)
                              qualify = map (uncurry sig)

sig clazz method = FQMethod (B.fqn clazz) (B.methodName method) (B.methodType method)

