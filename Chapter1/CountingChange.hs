denom 1 = 1
denom 2 = 5
denom 3 = 10
denom 4 = 25
denom _ = 50

cc 0 _ = 1
cc amount coin_types | (amount < 0 || coin_types ==0)  = 0
				     | otherwise = (cc amount (coin_types-1))  + (cc (amount - denom coin_types) coin_types)


count_change amount  =  cc amount 5
						
												
												