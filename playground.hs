(*>>) :: Applicative f => f a -> f b -> f b
fa *>> fb = (\_ x -> x) <$> fa <*> fb

myMapA :: Applicative f => (a -> f b) -> ([a] -> f [b]) 
myMapA f xs = foldr ((<*>) . fmap (:) . f) (pure []) xs

mySequenceA :: Applicative f => [f a] -> f [a]
mySequenceA = foldr ((<*>) . fmap (:)) (pure [])

myReplicateA :: Applicative f => Int -> f a -> f [a]
myReplicateA n fa = mySequenceA (replicate n fa)
