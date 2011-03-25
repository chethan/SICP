factorial x = factorial_iter 1 x
            	where factorial_iter acc num | num == 1 = acc
											 | otherwise = factorial_iter (acc * num) (num-1)