fib n = fib_iter 1 0 n
		where fib_iter a b n | n==0 = b
							 | otherwise = fib_iter (a+b) a (n-1)