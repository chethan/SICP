sieve [] = []
sieve (x:xs) = x : sieve (filter (\a -> not ((rem a x) == 0)) xs)
sieve_lazy xs = (\() -> sieve xs)