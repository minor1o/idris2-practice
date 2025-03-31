module L3 

%default total

c : Char 
c = '*'

d = ord c

s : String
s = "forty two"

cx : List Char 
cx = unpack s

-------------------


-- Types to str 
showBool: Bool -> String 
showBool False = "False"
showBool True = "True"
showChar: Char -> String 
showChar a = "'" ++ pack[a] ++ "'"

showNat: Nat -> String

showList': (a -> String) -> List a -> String
showList' show [] = ""
showList' show [x] = show x
showList' show (x :: xs) = show x ++ ", " ++ (showList' show xs)


showList: (a -> String) -> List a -> String
showList show [] = "[]"
showList show xs = "[" ++ (showList' show xs) ++ "]"



data Pair: Type -> Type -> Type where 
    MkPair : a -> b -> Pair a b 


p : (Bool, Nat)
p = (False, 4)
showPair: (a -> String) -> (b -> String) -> (a, b) -> String
showPair f g (x, y) = "(" ++ f x ++ "," ++ g y ++ ")"


%hide Show
-- haskell type class 
-- we will map type with function which converts this type to string
interface Show a where 
    total 
    show: a -> String

Show Bool where 
    show = showBool 

Show Char where 
    show = showChar

-- => is a special param: if for type a implemented intefrace, then 
-- function list of type to str 
showList'': Show a => List a -> String 
showList'' xs = showList show xs

Show a => Show (List a) where 
    show = showList''

Show a => Show b => Show (a, b) where
    show = showPair show show


%hide Eq
interface Eq a where
    total
    (==) : a -> a -> Bool
    (/=) : a -> a -> Bool
    
    -- Default implementation to make one of two impls
    x == y = not (x/=y)
    x /= y = not (x==y)


showSorted : Ord a => Show a => List a -> String  
showSorted' : {0 a : Type} -> {auto _ : Ord  a} -> {auto _ : Show a} -> List a -> String



-- Polymorphic binary tree 
-- required functions
-- Show
-- Eq
-- Number of elements 
-- min max element in tree
-- aboba for balancing 
-- adding elements to tree 
-- contains 

-- data Pair: Type -> Type -> Type where 
--     MkPair : a -> b -> Pair a b 
-- data Tree a = Leaf | Node a (List (Tree a)) where
--     MkPair : a -> b -> Pair a b 

data Tree : Type -> Type where 
    Node: a -> Tree a -> Tree a  -> Tree a
    Empty : Tree a 


size: Tree a -> Nat
size Empty = 0
size (Node x left right) = 1 + size left + size right

-- push: Ord a => Tree a -> b -> Tree a
-- push a: a < b -> Tree a -



t : Tree Nat 
t = Node 5 (Node 3 Empty (Node 4 Empty Empty)) (Node 7 (Node 6 Empty Empty) (Node 8 Empty Empty))



-- %hide Ord 
-- interface Ord Tree where
--     total
--     (>) : Tree  -> Tree  -> Bool
--     (<) : Tree  -> Tree  -> Bool
--     (>=) : Tree  -> Tree  -> Bool
--     (<=) : Tree  -> Tree  -> Bool
    


f : (a, b) -> c
g : a -> b -> c
-- carrying
cr : ((a, b) -> c) -> (a -> b -> c)

cr f = \x, y => f(x, y)

swap' : a -> b -> (b, a)
swap' = cr swap

uc : (a -> b -> c) -> ((a, b) -> c) 
uc f(x,  y) = f x y 
    