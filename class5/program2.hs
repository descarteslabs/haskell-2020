import qualified System.Environment as E
import qualified System.IO as IO

main:: IO ()
main = E.getArgs >>= maybeCopyFiles

maybeCopyFiles :: [String] -> IO ()
maybeCopyFiles args =
  if length args > 0
  then if "please" == last args
       then (copyishFiles id    $ init args)
       else (copyishFiles notId $      args) >> (putStrLn "Done! How'd that work for you?")
    else usage

copyishFiles :: (String -> String) -> [String] -> IO ()
copyishFiles f args =
  if length args == 2
  then (IO.readFile srcFile) >>= (return . f) >>= (\c -> IO.writeFile dstFile c)
  else usage
  where
    srcFile = args !! 0
    dstFile = args !! 1

notId :: String -> String
notId goodContent = badContent
  where
    goodLines = lines goodContent
    badLines = map reverse goodLines
    badContent = unlines badLines

usage :: IO ()
usage = putStrLn "Usage: program2 <src> <dst> please"

