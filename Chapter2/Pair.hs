make_pair x y = let 
			pick 1 = x
			pick 2 = y
		 in pick
		
car pair =  pair 1

cdr pair = pair 2	