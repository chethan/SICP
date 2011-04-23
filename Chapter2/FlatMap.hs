import Data.List

flatmap func seq = foldr (\x acc -> x ++ acc) [] (map func seq)

generate_seq n = flatmap (\x -> map (\y -> (y,x)) [1..(x-1)])[1..(n-1)]

my_permutations (x:[]) = [[x]]
my_permutations (xs) =  flatmap (\x-> (map (\y -> x:y) (my_permutations (delete x xs)))) (xs)

prime_sum_pairs n = let is_prime n = (rem ((n-1) ^ n) n) == (n-1)  
					in filter (\(a,b,c) -> is_prime c)(flatmap (\x -> map (\y -> (x,y,x+y)) [1..(x-1)])[1..(n)])

prime_sum_pairs_comprehensions n = let is_prime n = (rem ((n-1) ^ n) n) == (n-1)  
								   in [(i,j,i+j)| i <- [1..n], j <- [1..(i-1)] , is_prime (i+j)] 

