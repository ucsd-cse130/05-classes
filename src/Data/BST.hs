  module Data.BST where 

import qualified Test.QuickCheck as QC
import qualified Data.List       as L

-------------------------------------------------------------------------------
-- | BST data type 
-------------------------------------------------------------------------------

data BST a
  = Leaf                      -- ^ empty tree
  | Node a (BST a) (BST a)    -- ^ node with left and right subtrees
  deriving (Show)

-- | Binary-Search Ordering Invariant

isOrdered :: (Ord a) => BST a -> Bool
isOrdered Leaf         = True 
isOrdered (Node e l r) = forall l (\x -> x < e) -- all elts in `l` are less    than `e`
                      && forall r (\x -> e < x) -- all elts in `r` are greater than `e` 
                      && isOrdered l            -- left subtree `isOrdered`
                      && isOrdered r            -- right subtree `isOrdered` 

forall :: BST a -> (a -> Bool) -> Bool
forall Leaf         _ = True
forall (Node e l r) p = p e && forall l p && forall r p

-------------------------------------------------------------------------------
-- | The empty BST
-------------------------------------------------------------------------------
empty :: BST a
empty = Leaf 


-------------------------------------------------------------------------------
-- | Build a tree from a list
-------------------------------------------------------------------------------
build :: (Ord a) => [a] -> BST a
build xs = case (helper xs ) of 
  [] -> Leaf
  xs -> foldl f Leaf xs
     where
      f acc elem = nice acc elem
       where
        nice :: (Ord a) => BST a -> a -> BST a
        nice t x = case t of
         Node value left right -> if( value > x) then Node value (nice left x) right else Node value left (nice right x)
         Leaf -> Node x Leaf Leaf
  where
    helper::(Ord a) => [a] -> [a]
    helper xs = reverse (foldl f base fs)
     where
      f acc elem  =  (filter (/= elem) acc) ++ [elem]
      base = []
      fs = reverse xs
   
 

-------------------------------------------------------------------------------
-- | Check membership in BST
-------------------------------------------------------------------------------
contains :: (Ord a) => a -> BST a -> Bool
contains x t = case t of 
  Node value left right -> if value == x then True else if value > x then contains x left else contains x right
  Leaf-> False
  
t2 :: BST Int
t2 = Node 5 Leaf (Node 20 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf))


-------------------------------------------------------------------------------
-- | In-order traversal (fold)
-------------------------------------------------------------------------------
-- t is node value left right 

-- if empty return
-- inorder(root->left);
--inorder(root->right);


fold :: (b -> a -> b) -> b -> BST a -> b
fold f b t = case t of
  Leaf ->  b
  Node value left right -> final
   where
    leftFinal = fold f b left  
    final = fold f (f leftFinal value) right
     

toList :: BST a -> [a]
toList = reverse . fold (\xs x -> x:xs) []

toString :: (Show a) => BST a -> String
toString t = "build " ++ show (toList t) 


-------------------------------------------------------------------------------
-- | Adding an element
-------------------------------------------------------------------------------
add :: (Ord a) => a -> BST a -> BST a
add x t = case t of
   Node value left right -> if( value ==  x) then  Node value left right else if (value > x) then Node value (add x left) right else Node value left (add x right)
   Leaf -> Node x Leaf Leaf
   
     

-------------------------------------------------------------------------------
-- | Removing the minumum element
-------------------------------------------------------------------------------
removeMin :: (Ord a) => BST a -> (a, BST a)
removeMin t = case t of 
 Leaf -> error ( "Empty tree in removeMin")
 Node _ _ _ -> (findMin t, findTree t )
  where
   findMin :: (Ord a) =>  BST a -> a
   findMin tree = case tree of
    Node v l _ -> case l of
     Leaf -> v
     Node v1 l1 r1 ->  findMin (Node v1 l1 r1)
    Leaf -> error ( "Leaf: Something wrong in removeMin")
   
     
   findTree :: (Ord a) =>  BST a -> BST a
   findTree tree = case tree of
    Node v l r -> case l of
     Leaf -> r
     Node v1 l1 r1 ->  Node v (findTree (Node v1 l1 r1)) r
    Leaf -> error ( "Leaf: Something wrong in removeMin")

-------------------------------------------------------------------------------
-- | Removing an element
-------------------------------------------------------------------------------
remove :: (Ord a) => a -> BST a -> BST a
remove x t = case t of 
 Node _ _ _ -> if( mins ==  x) then minTree else helper (remove x minTree) mins
  where 
   (mins,minTree) = removeMin t
   helper :: (Ord a) => BST a -> a -> BST a
   helper tree v = case tree of
     Node value left right -> if( value > v) then Node value (helper left v) right else Node value left (helper right v)
     Leaf -> Node v Leaf Leaf
 Leaf -> t
-------------------------------------------------------------------------------
-- | QuickCheck Properties
-------------------------------------------------------------------------------

--  Holds after `build`
prop_build :: [Int] -> Bool
prop_build xs = isOrdered (build xs)

--  Holds after `contains` and `build`
prop_contains_elt :: Int -> [Int] -> Bool
prop_contains_elt x xs = (x `elem` xs) == (contains x (build xs))

--  Holds after `contains` and `fold`
prop_contains_elts :: BST Int -> Bool 
prop_contains_elts t = and [ contains x t | x <- toList t ] 

-- Holds after `add`
prop_add_elt :: Int -> BST Int -> Bool 
prop_add_elt elt t = contains elt (add elt t) 
  
-- Holds after `add`
prop_add_elts_old :: Int -> BST Int -> Bool 
prop_add_elts_old elt t = forall t (\x -> contains x t') 
  where 
    t'                  = add elt t   

-- Holds after `add`
prop_add_isOrd :: Int -> BST Int -> Bool
prop_add_isOrd elt t = isOrdered (add elt t)

-- Holds after `add`: Fix this property
prop_multiset :: [Int] -> Bool 
prop_multiset xs = toList (build xs) == helper (L.sort xs)   -- <<<< TBD: you need to fix this property
 where
  helper::[Int] -> [Int]
  helper xs = foldl f base fs
   where
    f acc elem  =  (filter (/= elem) acc) ++ [ elem]
    base = []
    fs = xs
  
  

-- Holds after `removeMin`
prop_remove_min :: BST Int -> Bool
prop_remove_min Leaf = True
prop_remove_min t    = contains x t && forall t' (\y -> x < y) 
  where 
    (x, t')          = removeMin t

-- Holds after `remove`
prop_remove :: Int -> BST Int -> Bool 
prop_remove elt t = not (contains elt t') 
  where 
    t'            = remove elt t 

-- Holds after `remove`
prop_remove_old :: Int -> BST Int -> Bool 
prop_remove_old elt t = forall t (\x -> x == elt || contains x t') 
  where 
    t'                = remove elt t 

-- Holds after `remove`
prop_remove_isOrd :: Int -> BST Int -> Bool
prop_remove_isOrd elt t = isOrdered (remove elt t)

-------------------------------------------------------------------------------
-- | QuickCheck Instance
-------------------------------------------------------------------------------
quickCheck :: (QC.Testable prop) => prop -> IO ()
quickCheck = QC.quickCheck

instance (Ord a, QC.Arbitrary a) => QC.Arbitrary (BST a) where
  arbitrary = build <$> QC.arbitrary
