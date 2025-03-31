module Lesson1
%default total

data Bit = B0 | B1
b0 : Bit

b0 = B0
b1 = B1

record Byte2 where 
    constructor MkByte2
    most: Bit
    least: Bit 

b10 : Byte2 
b10 = MkByte2 b1 b0 

data Bit' : Type where 
    B0' : Bit'
    B1' : Bit'

data Byte2' : Type where 
    MkByte2' : Bit' -> Bit' -> Byte2'


most : Byte2 -> Bit
least: Byte2 -> Bit

-- %default partial


-- partial

%default total 
notBit : Bit -> Bit 
notBit B0 = B1
notBit B1 = B0
-- notBit B1 = B0
-- notBit n = notBit n


idBit: Bit -> Bit
idBit x = x

-- lambda expression, anonymous function
idBit' : Bit -> Bit
idBit' = \n => n 

notbit'  : Bit -> Bit
notbit' = \n => case n of 
                    B0 => B1 
                    B1 => B0

-- axioms expressions for construction of Natural numbers
data Nat' : Type where
    Z' : Nat'
    S' : Nat' -> Nat'

z : Nat
z = Z

two : Nat 
two = S (S z)

data Bool' = False' | True' 

isEven : Nat -> Bool 
isEven 0 = True 
isEven 1 = False
isEven (S (S k)) = isEven (k)



-- divide by two 
collatz_even : Nat -> Nat
collatz_even 0 = 0 
collatz_even 1 = 1 
collatz_even n@(S (S k)) =  1 + collatz_even k 


-- 3n + 1 
colltaz_odd : Nat -> Nat
colltaz_odd k = 3 * k + 1 

collatz_step : Nat -> Nat  
collatz_step k = if isEven (k) then collatz_even k 
                               else colltaz_odd k

data LN : Type where
    ZL : LN 
    CL : Nat -> (LN -> LN)


collatz_show: Nat -> LN
collatz_show 0 = ZL
collatz_show n =                            



-- 3 случая: для 1 вернуть пустой список
-- для K $ head $ step k  