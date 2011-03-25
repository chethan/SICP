smallest_divisor n = find_divisor n 2
					 where find_divisor n divisor | (truncate (sqrt (fromIntegral n))) < divisor = n
						    					  | (rem n divisor)==0 = divisor
												  | otherwise =  find_divisor n (divisor+1)
												
is_prime n = n == smallest_divisor n												
												