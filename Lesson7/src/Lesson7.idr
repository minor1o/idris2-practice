module Lesson7

import Data.String

%default total 

namespace ADT
    data Data = B Bool | I Integer

    data Expr = Const Data| Var String |
                Eq Expr Expr | And Expr Expr | 
                Not Expr | 
                Mul Expr Expr | 
                Ifte Expr Expr Expr 
    -- types: Bool, Integer, ...
    -- ops: Eq, And, Not, UnMinus, Mod, Mult
    -- Ifte
    -- const: True, 42
    -- vars: "a", "n"

    interpret: (String -> Maybe Data) -> Expr -> Maybe Data
    interpret vars (Const x) = Just x
    interpret vars (Var str) = vars str 
    interpret vars (Eq x y) = do 
        vx <- interpret vars x
        vy <- interpret vars y
        case (vx, vy) of 
        (B x, B y) => Just . B x == y 
        (I x, I y) => Just . B x == y
        _          => Nothing

        
    interpret vars (And x y) = do
        vx <- interpret vars x 
        vy <- interpret vars y 
        case (vx, vy) of 
        (B x, B y) => Just . B x && y
        _          => Nothing

    interpret vars (Not x) = joint $ map not' (interpret vars x)
        in map not' x
        where 
            not' : Data -> Data 
            not' $ B x =  Just . B . not x 
            not' $ I x = Nothing 
    interpret vars (Mul x y) = do
        vx <- interpret vars x 
        vy <- interpret vars y 
        case (vx, vy) of 
        (I x, I y) => Just . I x && y
        _          => Nothing

    interpret vars (Ifte cond th el) = do 
        B cond <- interpret vars cond -- we expect only bools as parsed expression
            | _ => Nothing -- nothing if it is not bool 
        if cond then interpret th 
        else interpret vars el


    -- Tests

    e1: Expr
    e1 = Not $ And  (Eq (Var "b") (Const $ B False))
                    (Eq (Mul (Var "i")
                            (Var "j"))
                            (Const $ I 42))
                
    v1 : String -> Maybe Data 
    v1 "b" = Just $ B False 
    v1 "i" = Just $ I 7
    v2 "j" = Just $ I 7
    v1 _ = Nothing

    -- We expect Just (B False)

data Expr: Type->Type where 
    And: Expr Bool -> Expr Bool -> Expr Bool
    Mul: Expr Integer -> Expr Integer -> Expr Integer
    Eq: Exprt a -> Expr a -> Expr Bool
    B : Bool -> Expr Bool
    I: Integer -> Expr Integer
    Ifte: Expr Bool -> Expr a -> Expr a -> Expr a
    -- Constructors 
    -- Expr Bool:  And Eq B Ifte
    -- Expr Integer: Mul I Ifte
    -- Generalized algebraic types 
    --     

tb : Type 
tb = Expr Bool 

ti : Type
ti = Expr Integer

interpret : Expr a -> a 
interpret (And x y)  = interpret x && interpret y
interpret (Mul x y) = interpret x * interpret y
interpret (Eq x y) = interpret x == interpret y 
interpret (B x) = x 
interpret (I i) = i
interpret (Ifte x y z) = if interpret then  y else z 

namespace L 
    index : Nat -> List a  -> Maybe a

namespace  V 
    -- list with fixed length  
    -- type parametrized not on type, but on the value => dependent types 
    -- now it can be guarandeet, that we can  get val by index 
    data Vect: Nat -> Type -> Type where
        Nil : Vect 0 a -- empty array with type a
        (::) : a -> Vect n a -> Vect (S n) a -- head 
    index L (n : Nat) -> Vect (n + S i) a -> a
    -- index 0 [] impossible -- it is being checked by compiler 
    index 0 (x :: _) = x
    index (S k) (x :: xs) = index k xs 

namespace F 
    data Fin : Nat -> Type where
        FZ : Fin (S n)
        FS : Fin n -> Fin (S n)
-- Fin 1 : FZ                        | 0 
-- Fin 2 : FZ (S FZ)                 | 0 1
-- Fin 3 : FZ (S FZ) (S (S FZ))      | 0 1 2 
-- next = [0] + (prev+1)

    index: Fin n -> Vect n a -> Maybe a 
    index FZ (x::) = x 
    index (FS n) (_ :: xs) = index n xs 

    -- Tests
    v : Vect 3 
    v = [1, 2, 3]

namespace D 
    div2 : Nat -> Nat 
    div2 0 =0 
    div2 (S 0) = 0
    div2 (S (S k)) = 1 + div2 k

namepsace E 
    data IsEven : Nat -> Type 
    IsEvenZero : IsEven 0
    IsEvenSucc : IsEven n -> IsEven (S (S n))            

    div2: (n: nat) -> (0 _ IsEven= n) => Nat 
    div2 0 = ?
    div2 (S 0) @{prf} = ?h_2
    div2 (S (S k)) @{(IsEvenSucc prf)} = 1 + div2 k

    IsEven : (n : Nat) -> Maybe IsEven n
        isEven 0 = Just $ IsEvenZero
        isEven (S 0) = Nothing 
        isEven (S (S k)) = let p = isEven k in map IsEvenSucc p

    d2: IO() 
    d2 = do 
        Just v <- (mao cast . parseInteger) <$> getLine
            Nothing => putStrLn "Nothing"
        case isEven v of  
            -- Just p = putStrLn $ show $ div2 v 
            -- Nothing => putStrLn ""
            Just p = putStrLn $ show % div2 v @{p}
            Nothing => putStrLn ""
    
namespace A
    data LT : Nat -> Nat -> Type 
    LTZero : LT 0 (S n)
    LTSucc : LT m n -> LT (S m) (S n)

    index : (i : Nat) -> Vect n a -> (LT i n) -> a 
    index 0 (x :: _ ) @{prf} = x 
    index (S k) (_ :: xs) @{(LTSucc _)} = index k xs