smallest_divisor n next  = find_divisor n 2 next
					 		where find_divisor n divisor next | (truncate (sqrt (fromIntegral n))) < divisor = n
						    					  		      | (rem n divisor)==0 = divisor
												  	          | otherwise =  find_divisor n (next divisor) next
get_next 2 = 3
get_next x = x+2

is_prime n = n == smallest_divisor n (1+)

is_fast_prime n = n == smallest_divisor n get_next
						
						


												
												