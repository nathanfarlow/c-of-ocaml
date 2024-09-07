let rec f k = if k < 2 then 1 else f (k - 1) + f (k - 2)
