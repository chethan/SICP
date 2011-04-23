cons x y = (\m -> m x y)
car x = x (\a b -> a)
cdr x = x (\a b -> b)