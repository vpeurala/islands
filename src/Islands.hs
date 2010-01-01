import System.Directory

main = do doesConfigFileExist <- doesFileExist ".islands"
          putStrLn $ show doesConfigFileExist
