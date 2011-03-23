factorial x = factorial_iter 1 1 x
            	where factorial_iter acc count max_count | count > max_count = acc
														 | otherwise = factorial_iter (acc * count) (count+1) (max_count)