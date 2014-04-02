

-- Remove duplicates
rmd :: [Int] -> [Int]
rmd [] = []
rmd (x:xs) = x:[n | n <- rmd xs , n /= x]

main = print (rmd [1,2,1,2,3,3,3,3,2,2,2,2,1,2,3,1,1])
