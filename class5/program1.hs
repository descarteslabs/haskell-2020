import qualified System.Environment as E
import qualified System.IO as IO

main:: IO ()
main = E.getArgs >>= copyFiles

copyFiles :: [String] -> IO ()
copyFiles args =
  if length args == 2
  then (IO.readFile srcFile) >>= (\c -> IO.writeFile dstFile c)
  else usage
  where
    srcFile = args !! 0
    dstFile = args !! 1

usage :: IO ()
usage = putStrLn "Usage: program1 <src> <dst>"

