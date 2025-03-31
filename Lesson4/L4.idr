module Lesson4 
import Data.String

%default total 

cvt: String -> Double 

map' : (a -> b) -> List a -> List b
map' f [] = []
map' f (x :: xs) = f x :: map' f xs


ss: List String
ss = ["1", "42"]

%hide Cast
%hide cast 
-- required conversions from a to b, but not from b to a 

failing "Ambiguous"
intefrace Cast a b where 
    cast : a -> b


failing "Ambiguous"
ints: List int 
ints = map' cast ss

dbls : List Double 
dbls = map' (sqrt.cast) ints


ints' : List (Maybe Integer)
ints' : map' ParseInteger ss


map'': (Integer -> Double) -> Maybe Integer -> Maybe Double
map'' f Nothing = Nothing 
map'' f (Just x) = Just $ f x 



dbls' : List (Maybe Double)
dbls' = map' (appplyMaybe $ sqrt.cast) ints'

sqrt' : Double -> Maybe Double
sqrt' dbl = if dbl < 0 then Nothing else Just $  sqrt dbl 

dbls'' : List (Maybe Double) 
dbls'' = map' (map'' $ sqrt' . cast) ints' 

data Either e a = Left e | Right a


parseInteger' : String  -> Either String Integer
parseInteger' s = case parseInteger of 
                  Just i => Right i 
                  Nothing => Left $ "Failed to parse integer: " ++ s
        

sqrt'' : Double -> Either String Double 
sqrt'' dbl = if dbl < 0 then Left "Cannot compute square root of negative number" 
                       else Right $  sqrt dbl


dbls''' : List (Either String $ Either String)


map: (a -> b) -> f a -> f b
-

mapF : {0 a : Type} -> f a -> f b 

failing 
    interface Functor f where
    map: (a -> b) -> (f a -> f b) 

mapPair : (a -> b) -> (e, a) -> (e, b) 

-- map id = id
-- map (p . q) = (map p) . (map q) 

($) : (a -> b) -> a -> b
(<$>) : Functor f => (a -> b) -> f a -> f b


record  Morhphism e a where 
    constructor MkMorphism
    m : e -> a

Functor (Morhpishm e) where
    map f (MkMorphism m) = MkMorphisms (f . m)


map2 : Functor g => ( a -> b -> c) -> (g a -> g b  -> g c) 
map2 f x = ?h
-- map f x : g (b  -> c)
f <$> x  <*> y (b -> c)

interface Functor f => Applicative f where
    (<*>) :{0 g : Type -> Type} g (a -> b) -> g a -> g b 
    pure f: (a -> b) -> f( a -> b)

x : Maybe Nat 
-- something multiply by 2 -> Nat 
-- and then multiply by 3 
x = Just (*2) <*> Just 3 
-- result is Just 6

y : List Nat 
y = [(*2), (*3)] <*> [1,2, 3,4]         