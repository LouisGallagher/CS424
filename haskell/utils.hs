

foo f [] ys = []
foo f (x:xs) ys = [f x] ++ foo f ys xs