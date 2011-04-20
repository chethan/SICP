element_of_set elem (xs) = foldr (\x acc -> acc || (x==elem)) False xs

adjoin_set elem xs | (element_of_set elem xs) = xs
				   | otherwise = elem:xs
				   
intersection_set set1 set2 = let 
							  step x acc | element_of_set x set1 = x:acc
										 | otherwise = acc
						    in foldr step [] set2
							
union_set set1 set2 = foldr (\x acc -> adjoin_set x acc) set2 set1


element_of_ordered_set elem [] = False
element_of_ordered_set elem (x:xs) |(elem == x) = True
								   | elem < x = False
								   | otherwise = element_of_ordered_set elem xs

intersection_ordered_set _ [] = []								   
intersection_ordered_set [] _ = []								   
intersection_ordered_set (x:xs) (y:ys) |  (x==y) = x : 	intersection_ordered_set xs ys							   
								       |  (x < y) = intersection_ordered_set xs (y:ys)
								       |  (y < x) = intersection_ordered_set (x:xs) ys
									   
														
union_ordered_set [] s2 = s2								   
union_ordered_set s1 [] = s1								   
union_ordered_set (x:xs) (y:ys) |  (x==y) = x : union_ordered_set xs ys							   
								|  (x < y) = x : union_ordered_set xs (y:ys)
								|  (y < x) = y : union_ordered_set (x:xs) ys
									   
														