import Data.Array

fibo a b n = fibs ! (n-1)
      where fibs = listArray (0,n) $ a:b:map f [2..]
            f n = fibs ! (n-1) ^ 2 + fibs ! (n-2)


main :: IO()
main = do
  line <- getLine
  let [a,b,n] = map read (words line)
  print $ fibo a b n
