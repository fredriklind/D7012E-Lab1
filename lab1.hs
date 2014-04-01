add :: Int -> Int -> Int
add a b = a+b

square :: Int -> Int
square n = n*n

-- Adds two numbers, then squares them.
addSquare :: Int -> Int -> Int
addSquare a b = square (add a b)

main = print (addSquare 2 3)
