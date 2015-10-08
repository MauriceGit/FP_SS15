

-- foldr Implementierung
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f e [] = e
foldr' f e (x:xs) = f x (foldr' f e xs)

-- foldl Implementierung
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f e [] = e
foldl' f e (x:xs) = foldl' f (f e x) xs

-- map mit foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr' (\ x xs -> (f x) : xs) [] 

-- map mit foldl
map'' :: (a -> b) -> [a] -> [b]
map'' f = foldl' (\ xs x -> (f x) :xs) [] 

-- reverse mit foldr
reverse' :: [a] -> [a]
reverse' = foldr' (\ x xs -> xs ++ [x]) []

-- reverse mit foldl
reverse'' :: [a] -> [a]
reverse'' = foldl' (\ xs x -> x : xs ) []

-- tricky reverse
reverse''' :: [a] -> [a]
reverse''' = foldl' (flip (:)) []









































































