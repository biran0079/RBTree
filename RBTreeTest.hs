import RBTree
import Data.Set (elems, Set)
import Test.QuickCheck

toList Nil = []
toList (Node _ x left right) = toList left ++ [x] ++ toList right

isRB' Nil = (True, 0)
isRB' (Node color v l r) = (lrb && rrb && rh == lh, lh + (height color))
  where (lrb, lh) = isRB' l
        (rrb, rh) = isRB' r
        height Black = 1
        height Red = 0

isSorted []       = True
isSorted [x]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

isRB t = isBlack t && (fst $ isRB' t) && (isSorted $ toList t)


prop_insertRBTree :: Set Int -> Bool
prop_insertRBTree input =
  let
    lst = elems input
    tree = foldr insert Nil lst
  in 
    isRB tree && (toList tree) == lst 

prop_deleteRBTree :: Set Int -> Bool
prop_deleteRBTree input =
  let
    lst = elems input
    n = (length lst `div` 2)
    toDelete = take n lst
    remaining = drop n lst
    tree = foldr delete (foldr insert Nil lst) toDelete
  in 
    isRB tree && (toList tree) == remaining

main :: IO () 
main = do 
    quickCheck prop_insertRBTree
    quickCheck prop_deleteRBTree
    return ()
