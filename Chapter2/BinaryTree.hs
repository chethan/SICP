data BinaryTree a = Node a (BinaryTree a) (BinaryTree a)
			      | Empty
				    deriving (Show)
					
in_order (Node a Empty Empty) = [a]
in_order (Node a left_tree right_tree) =  in_order (left_tree) ++ [a] ++ in_order (right_tree)
in_order (Empty) = []