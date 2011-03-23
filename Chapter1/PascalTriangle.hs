pascal_term _ 1 = 1
pascal_term m n | m ==n = 1
				| m < n = 1
				| otherwise = pascal_term (m-1)(n-1) + pascal_term (m-1)(n)
				
pascal_temp 1 1 = [1]
pascal_temp m 1 = (pascal_temp (m-1) (m-1)) ++ [1]
pascal_temp m n = (pascal_temp (m) (n-1)) ++ [pascal_term m n]				
				
pascal_triangle n = pascal_temp n n
					 
						