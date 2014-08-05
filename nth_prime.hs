nth_prime n = selectors generator
	where 
		generator = [2..]
		selectors = (!!n) . (foldr sieve [])
		--sieve x [] = [x]
		sieve x any = x : (filter ((0 /=). (`mod` x)) any)