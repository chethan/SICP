fixed_point f guess = let 
						tolerance = 0.00001
						close_enough a b = abs (a-b) < 0.00001
						try guess | close_enough guess (f guess) = f guess	
						 		  | otherwise = try (f guess)
					in try guess
					
average_damp f = \(x) -> ((x + f x)/2)

derive f = \(x) -> (((f(x+dx)) - (f x)) / dx)
			where dx = 0.00001
			
newtons_method f guess = fixed_point (newtons_transform f) guess
						 where newtons_transform f = \(x) -> (x - (f x) / (derive f x))
						
sqrt_one x = fixed_point (average_damp (\(y)->(x/y))) 1

sqrt_two x = newtons_method (\(y)-> (y*y) - x) 1

	