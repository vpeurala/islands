import qualified Data.Map as Map
import Data.Map (Map, (!))
import System.Directory
import Text.Regex

data Config = Config {
      classpathRoots :: [String]
    , executionRoots :: [String]                        
      } deriving (Show)

main = do doesConfigFileExist <- doesFileExist ".islands"
          putStrLn $ show doesConfigFileExist

--parse :: String -> Map String [String]
parse input = mkConfig $ Map.fromList $ map (mkConfigLine . splitLine) (lines input)
    where parseValues = splitRegex (mkRegex ",")
          mkConfigLine (k, v) = (k, parseValues v)
          mkConfig :: Map String [String] -> Config 
          mkConfig m = Config (m ! "classpath_roots") (m ! "execution_roots")

splitLine :: String -> (String, String)
splitLine l = let (v1 : v2 : []) = splitRegex (mkRegex "=") l
            in (v1, v2)
