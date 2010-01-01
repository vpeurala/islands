module Islands where

import qualified Data.Map as Map
import Data.Map (Map, (!))
import System.Directory
import Text.Regex
import qualified Data.ByteString.Lazy as L
import qualified Islands.Bytecode as B
import Control.Monad (filterM, join, forM)
import System.FilePath ((</>))

data Config = Config {
      classpathRoots :: [FilePath]
    , executionRoots :: [FilePath]
      } deriving (Show)

main = do doesConfigFileExist <- doesFileExist ".islands"
          file <- readFile ".islands"
          let classes = map classesInDir (classpathRoots $ parse file)
          (head classes) >>= putStrLn . show
              where classesInDir :: FilePath -> IO [B.Class]
                    classesInDir d = do files <- classFiles d 
                                        sequence $ map parseClass files
                    parseClass f = do bytes <- L.readFile f
                                      return (B.parse bytes)

classFiles :: FilePath -> IO [FilePath]
classFiles path = do files <- getRecursiveContents path 
                     return (filter isClassfile files) 
    where isClassfile name = (take 6 $ reverse name) == "ssalc." -- FIXME cleanup

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do names <- getDirectoryContents topdir
                                 let properNames = filter (`notElem` [".", ".."]) names
                                 paths <- forM properNames $ \name -> do
                                            let path = topdir </> name
                                            isDirectory <- doesDirectoryExist path
                                            if isDirectory
                                              then getRecursiveContents path
                                              else return [path]
                                 return (concat paths)

parse :: String -> Config
parse input = mkConfig $ Map.fromList $ map (mkConfigLine . splitLine) (lines input)
    where parseValues = splitRegex (mkRegex "\\s*,\\s*")
          mkConfigLine (k, v) = (k, parseValues v)
          mkConfig m = Config (m ! "classpath_roots") (m ! "execution_roots")

splitLine :: String -> (String, String)
splitLine l = let (v1 : v2 : []) = splitRegex (mkRegex "\\s*=\\s*") l
            in (v1, v2)
