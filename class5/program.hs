import qualified System.Environment as E

main:: IO ()
main = E.getArgs >>= maybeCoypeFiles

