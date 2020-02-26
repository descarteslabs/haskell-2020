removeWhiteSpace = f . f
  where f = (dropWhile ((==) ' ')) . reverse

testInput = "  test input  "
testOutput = removeWhiteSpace testInput
