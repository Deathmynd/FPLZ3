data ReverseList a = RNil | RCons (ReverseList a) a
-- util
showAsList :: (Show a) => ReverseList a -> String
showAsList RNil = ""
showAsList (RCons RNil h) = show h
showAsList (RCons tl hl) = (showAsList tl) ++ ", " ++ (show hl)

-- instances
toList :: ReverseList a -> [a]
toList RNil = []
toList (RCons t h) = h:(toList t)

fromList :: [a] -> ReverseList a
fromList [] = RNil
fromList (h:t) = RCons (fromList t) h

instance (Eq a) => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) RNil _ = False
    (==) _ RNil = False
    (==) (RCons tl hl) (RCons tr hr) = if (hr == hl)
        then (tr == tl)
        else False

instance (Ord a) => Ord (ReverseList a) where
    (<=) RNil RNil = True
    (<=) RNil _ = True
    (<=) _ RNil = False
    (<=) (RCons tl hl) (RCons tr hr) = if (hl > hr) 
        then False
        else tl <= tr

instance (Show a) => Show (ReverseList a) where
    show RNil = "[]"
    show list = "[" ++ (showAsList list) ++ "]"

instance Monoid (ReverseList a) where
    mempty = RNil
    mappend RNil list = list
    mappend list RNil = list
    mappend list (RCons tl hl) = RCons (mappend list tl) hl

instance Functor ReverseList where
    fmap f RNil = RNil
    fmap f (RCons tl hl) = RCons (fmap f tl) (f hl)

--main
test :: String -> String
test str =
    let x = (RCons (RCons (RNil) 2) 3) in
    let y = (RCons (RCons (RNil) 2) 3) in
    let t = fmap (\a -> a +1) x in-- mappend x y in --if (x <= y) then True else False in
    show $ t
main = interact test