map_using_fold f xs =  foldr (\x acc -> (f x):acc) [] xs
append_using_fold xs ys =  foldr (\x acc -> x:acc) ys xs
length_using_fold xs = foldr (\x acc -> 1+acc) 0 xs
hornets_rule xval coeff = foldr (\x acc -> x +(acc * xval)) 0 coeff
reverse_using_foldr xs = foldr (\x acc ->  acc ++ [x]) [] xs
