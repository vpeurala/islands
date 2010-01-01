import qualified Data.Map as Map
import System.Directory
import Text.Regex

data Config = Config {
      classpathRoots :: [FilePath]
    , executionRoots :: [FilePath]                        
      } deriving (Show)

main = do doesConfigFileExist <- doesFileExist ".islands"
          putStrLn $ show doesConfigFileExist

--parse :: String -> Map String [String]
parse input = Map.fromList $ map (\l -> mkConfig $ splitLine l) (lines input)
    where parseValues = splitRegex (mkRegex ",")
          mkConfig (k, v) = (k, parseValues v)
         

splitLine :: String -> (String, String)
splitLine l = let (v1 : v2 : []) = splitRegex (mkRegex "=") l
            in (v1, v2)
