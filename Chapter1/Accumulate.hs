accumulate combiner null_value term a next b
		  		  | (a > b) =  
					null_value
				  | otherwise = 
					combiner (term a) (accumulate combiner null_value term (next a) next b)
					
accumulate_iter combiner null_value term a next b =
		let iter low acc |(low > b) = 
								acc
						 |otherwise = 
								iter (next low) (combiner acc (term low))
	 in iter a null_value
								
								