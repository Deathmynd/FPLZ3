newtype PSet a = PSet {contains :: (a->Bool)}
newtype PSet2 a = PSet2 {contains2 :: (a->Bool)}

--Булевы значения, False и логическое «или»
instance Monoid (PSet a) where
	mempty = PSet (\x -> False)
	mappend (PSet f1) (PSet f2) = PSet (\x -> (f1 x) || (f2 x))
	
--Булевы значения, True и логическое «и»
instance Monoid (PSet2 a) where
	mempty = PSet2 (\x -> True)
	mappend (PSet2 f1) (PSet2 f2) = PSet2 (\x -> (f1 x) && (f2 x))
	
--функтор позволяет произвести операцию над каждым элементом типа,
--а так как у нас в типе хранится только функция,
--мы применяем функцию функтора к результату функции типа.
instance Functor PSet where
fmap f (PSet x) = PSet(\a -> f $ x a)

--main
test :: String -> String
test str = 
	let t = "Hello" in
	show $ t
main = interact test