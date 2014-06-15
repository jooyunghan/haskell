
count_bst = (map cbst [0 .. ] !!)
	where
		cbst 0 = 1
		cbst 1 = 1
		cbst n = sum [(count_bst i) * (count_bst (n-1-i)) | i <- [0..n-1]] 

main = do
	putStrLn $ show $ count_bst 10