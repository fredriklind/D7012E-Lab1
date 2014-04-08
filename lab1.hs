import Data.List

kmaxsubunique :: [Int] -> Int -> [(Int,Int,Int)]
kmaxsubunique x k = kmaxsub (rmd x) k
    where rmd [] = []
          rmd (x:y) = x:[i | i <- rmd y , i /= x]                       -- Remove duplicates
          kmaxsub x k = take k(                                         -- Get the first k triples
                reverse(                                                -- Reverse the list
                    sort(                                               -- Sort the list
                        [(v,i,j) |                                      -- Do a list comprehension on all possible subsequences
                            n <- subsequences x,                        -- Get all subsequences
                                let v = sum n                           -- Sum of the subsequence
                                    i = head(elemIndices (head n) x)+1  -- Find the index of the first element in the subsequence in the original list
                                    j = head(elemIndices (last n) x)+1, -- Find the index of the last element in the subsequence in the original list
                            n `isInfixOf` x]                            -- Only include continuous subsequences
                            )
                    )
                )