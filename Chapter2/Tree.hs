data Tree a = Node a [Tree a]
            | Empty
			  deriving (Show)
			  
count_leaves (Node a [Empty]) = 1 			  			  
count_leaves (Node a (xs)) = sum (map count_leaves xs) 			  
count_leaves (Empty) = 0 			  

pre_order (Node a [Empty]) = [a] 			  			  
pre_order (Node a xs) = foldl (\acc x-> acc ++ (pre_order x)) [a] xs 			  
pre_order (Empty) = []

post_order (Node a [Empty]) = [a] 			  			  
post_order (Node a xs) = (foldl (\acc x-> acc ++ (post_order x)) [] xs ) ++ [a]
post_order (Empty) = []


 			  

