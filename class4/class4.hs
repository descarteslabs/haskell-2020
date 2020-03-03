-- Lets import a module called Data.Map. We'll rename it as DM
import qualified Data.Map as DM

-- DM contains functions called empty, insert and lookup. Let's try
-- them out.
map0 = DM.empty
map1 = DM.insert 1 "Engineering" map0
map2 = DM.insert 2 "Applied Science" map1
result1 = DM.lookup 1 map2
result2 = DM.lookup 3 map2

-- Lets look at the return type for lookup.

-- The type keyword provides a means to create type aliases. Here
-- we've partially evaulated the type constructor DM.Map by supplying
-- the first type.
type IntMap a = DM.Map Int a

map0' = DM.empty :: IntMap a
map1' = DM.insert 1 "Haskell Class" map0'

--------------------------
-- Recursive data types --
--------------------------

-- Lets create a Binary tree type, and make it an instance of both
-- Show and Read typeclasses.
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read)

treeInsert EmptyTree x = Node x EmptyTree EmptyTree
treeInsert (Node y left right) x =
  if x < y
  then Node y (treeInsert left x) right
  else Node y left (treeInsert right x)

testTree = foldl treeInsert EmptyTree "Descartes Labs loves Haskell"

-- Lets make Tree a better instance of Show

-- Lets make Tree a better instance of Read

--------------
-- Functors --
--------------

-- Idea: fmap applied to Maybe handles the case when the value is
-- Nothing.
v1 = fmap (+1) $ Just 7
v2 = fmap (+1) Nothing 

v3 = (+1) <$> Just 7
v4 = (+1) <$> Nothing

-- Think of functors as a context.

v5 = (+1) <$> [1..10]

--------------------------
-- Applicative Functors --
--------------------------

-- What if the function doing the mapping resides in a context? We use Applicative functors
v6 = Just (+1) <*> Just 8
v7 = [(+1), (*2)] <*> [1..10]
