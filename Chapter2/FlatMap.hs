import Data.List

flatmap func seq = foldr (\x acc -> x ++ acc) [] (map func seq)

generate_seq n = flatmap (\x -> map (\y -> (y,x)) [1..(x-1)])[1..(n-1)]

my_permutations (x:[]) = [[x]]
my_permutations (xs) =  flatmap (\x-> (map (\y -> x:y) (permutations (delete x xs)))) (xs)