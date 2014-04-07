import Data.List


-- Remove duplicates
rmd :: [Int] -> [Int]
rmd [] = []
rmd (x:y) = x:[i | i <- rmd y , i /= x]

kmaxsub :: Int -> [Int] -> [(Int,Int,Int)]
kmaxsub k x = take k(reverse(sort([(v,i,j) | n <- subsequences x, 
                                    let v = sum n
                                        i = head(elemIndices (head n) x)+1
                                        j = head(elemIndices (last n) x)+1,
                                        n `isInfixOf` x])))

kmaxsubunique :: Int -> [Int] -> [(Int,Int,Int)]
kmaxsubunique k x = kmaxsub k (rmd x)


-- Remove duplicates
-- Get all possible subsequences
-- With a list comprehension, select only continuous subsequences
-- During the list comprehension, compile the tripe with sum, start index and end index
-- Remove the first element (it's the empty list)
-- Sort the resulting list
-- Reverse that list
-- Return the k first elements