-- Apply a function to a list
-- Can be used to move all attackers position
-- outputs> [2, 3, 4, 5, 6]
map (+1) [1..5]


-- Always try to prepend instead of append as the former is faster
-- outputs> [1, 2, 3, 4, 5]
-- new_element : [old list]
1 : [2..5]
-- instead of [old list] ++ new_element
[1..4] ++ 5


-- Search within a list of tuples
-- outputs> "lorem"
listName = [(42, "lorem"), (1337, "ipsum")]
lookup 42 listName


-- Concatenate 2 lists into one
-- outputs> [(1, "a"), (2, "b"), (3, "c")]
zip [1, 2, 3] ["a", "b", "c"]


-- Recursion
-- The function can for example accept 2 arguments. The first one is a boolean expression and the second one is a list
-- The following is the guard to stop it if the list is empty
-- Note that _ is a wildcard
dropItWhile _ [] = []
-- Then, the function can be the following
-- Note that we can name the parameters as bool and xxs for example
-- In addition, we can take the head of the list and therefore its tail by naming them conventionaly x and xs
-- The if statement apply the boolean expression to the x (the head of the list xxs) and do recursion if true or return the whole list
dropItWhile bool xxs@(x:xs) =
    if bool x then dropItWhile bool xs else xxs


-- Specify the type of parameters expected and the return type too
-- Here a list of Ints which outputs just an Int
functionName :: [Int] -> Int
-- If there is several parameters, the signature will be as follow:
functionName :: Int -> [Char] -> (Int, [Char])


-- Create custom types
type Username = [Char]
type Id = Int
type User = (Id, Username)
-- Then use it in a function
-- outputs a tuple of Id and Username, also defined as a tuple of User
functionName :: Id -> Username -> User
functionName i u = (i, u)


-- Create a collection of data
-- Note that it needs to inherit from Show if we want to actually display its elements
data User = Users { userId          :: Int
                  , userName        :: String
                  , userPassword    :: String
                  } deriving (Show)


-- Display a list of values
-- outputs> "Hello"
-- outputs> "world"
mapM_ print ["Hello", "world"]




















