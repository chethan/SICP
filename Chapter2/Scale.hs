scale [] _ = []
scale (x:xs) value = (x*value):(scale xs value)

my_map [] _ _ = []
my_map (x:xs) f value = (f x value):(my_map xs f value)

scale_map (x:xs) value = my_map (x:xs) (*) value
