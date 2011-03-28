let testing = 'git testing in branch'
square_root  x = let  
				  improve_guess guess x = average guess ( x / guess)
				  average x y = (x+y)/2
				  good_enough guess x = (abs((guess * guess) - x)) > 0.0001
				  square_root_temp guess x | (good_enough guess x) =  square_root_temp (improve_guess guess x) x
 												| otherwise = guess
                in
					square_root_temp (x/2) x
