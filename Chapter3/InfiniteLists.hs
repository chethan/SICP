fibs = 0 : 1 : zipWith (+)	fibs (tail fibs)
ints = 1 : zipWith (+)	ints [1,1..]
partial_sums = 1 : zipWith (+) partial_sums (tail ints)
