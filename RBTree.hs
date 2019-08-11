module RBTree where

data RBTree a = Nil | Node Color a (RBTree a) (RBTree a) deriving(Show)
data Color = Red | Black deriving (Show, Eq)

member :: (Ord a) => a -> RBTree a -> Bool
member _ Nil = False
member x (Node _ y left right)
    | x < y = member x left
    | x > y = member x right 
    | otherwise = True

blacken :: RBTree a -> RBTree a
blacken Nil = Nil
blacken (Node _ value left right) = Node Black value left right

insert :: (Ord a) => a -> RBTree a -> RBTree a
insert x root = blacken $ insert' root
  where insert' Nil = Node Red x Nil Nil
        insert' root@(Node color y left right) 
            | x < y = balance color y (insert' left) right
            | x > y = balance color y left (insert' right)
            | otherwise = root

balance :: Color -> a -> RBTree a -> RBTree a -> RBTree a
balance Black z (Node Red x a (Node Red y b c)) d = Node Red y (Node Black x a b) (Node Black z c d)
balance Black z (Node Red y (Node Red x a b) c) d = Node Red y (Node Black x a b) (Node Black z c d)
balance Black x a (Node Red y b (Node Red z c d)) = Node Red y (Node Black x a b) (Node Black z c d)
balance Black x a (Node Red z (Node Red y b c) d) = Node Red y (Node Black x a b) (Node Black z c d)
balance color value left right = Node color value left right

isBlack :: RBTree a -> Bool
isBlack (Node Red _ _ _) = False
isBlack _ = True

balL :: Color -> a -> (RBTree a, Bool) -> RBTree a -> (RBTree a, Bool)
balL color y (left, balanced) right = if balanced then (Node color y left right, True) else balL' color y left right

balR :: Color -> a -> RBTree a -> (RBTree a, Bool) -> (RBTree a, Bool)
balR color y left (right, balanced) = if balanced then (Node color y left right, True) else balR' color y left right

balL' :: Color -> a -> RBTree a -> RBTree a -> (RBTree a, Bool)
balL' color1 p n (Node color2 s sl sr) 
    | color2 == Red = balL Black s (balL' Red p n sl) sr
    | isBlack sl && isBlack sr = (Node Black p n (Node Red s sl sr), color1 == Red)
    | not (isBlack sr) = (Node color1 s (Node Black p n sl) (blacken sr), True)
    | otherwise = let (Node Red x sll slr) = sl in balL' color1 p n (Node Black x sll (Node Red s slr sr))
      
balR' :: Color -> a -> RBTree a -> RBTree a -> (RBTree a, Bool)
balR' color1 p (Node color2 s sl sr) n 
    | color2 == Red = balR Black s sl (balR' Red p sr n)
    | isBlack sl && isBlack sr = (Node Black p (Node Red s sl sr) n, color1 == Red)
    | not (isBlack sl) = (Node color1 s (blacken sl) (Node Black p sr n), True)
    | otherwise = let (Node Red x srl srr) = sr in balR' color1 p (Node Black x (Node Red s sl srl) srr) n

delete :: (Ord a) => a -> RBTree a -> RBTree a 
delete x t = fst $ delete' x t
  where
    delete' x Nil = (Nil, True)
    delete' x t@(Node color y left right)
        | x < y = balL color y (delete' x left) right
        | x > y = balR color y left (delete' x right)
        | otherwise = deleteRoot t
    deleteRoot (Node color _ Nil Nil) = (Nil, color == Red)
    deleteRoot (Node _ _ left Nil) = (blacken left, True) 
    deleteRoot (Node _ _ Nil right) = (blacken right, True)
    deleteRoot (Node color _ left right) = let m = findMin right in balR color m left (delete' m right)
    findMin (Node _ x Nil _) = x
    findMin (Node _ _ left _) = findMin left

