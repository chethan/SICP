h a b n = (b-a)/n

yk f a h k = (f (a+ (k*h)))

summation func low high acc next | low > high = acc
 							     | otherwise = summation func (next low) high (acc+(func low)) next

integral func low high n = let 
							h_temp = h low high n
							yk_temp = yk func low h_temp
							odd_sum = summation yk_temp 1 (n-1) 0 (2+)
							even_sum = summation yk_temp 2 (n-1) 0 (2+) 
						  in (h_temp/3) * ((yk_temp 0) + (yk_temp n) + (4 * odd_sum) + (2 * even_sum)) 
								  