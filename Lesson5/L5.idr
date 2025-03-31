module Lesson5 

import Control.Monad.Reader
import Data.List
import Control.Monad.Identity

%default total 

namespace MyIO
    data IO : Type -> Type
    -- data Unit : MkUnit 
    -- data Void : Type
    -- f: a -> Void 

getLine' : IO String 

putStrLn' : String -> IO ()

-- combinator 
(>>) : m () -> m b -> m b 


putStrLn2 : String -> String -> IO ()
putStrLn2 s1 s2 = putStrLn s1 >> putStrLn s2

-- failing 
    -- another combinator
    -- (>>=) : IO a -> (a -> IO b) -> IO b


readAndPut: IO () 
readAndPut = getLine >>= putStrLn

Functor IO:
map : Functor IO => (a -> b) -> IO a -> IO b

-- getLine' : IO String 
-- reverse: String -> String 

getReversedLine: IO String                 
-- getReversedLine = map reverse getLine       
-- execute map
-- getReversedLine = reverse <$> getLine
getReversedLine = getLine <&> reverse

getReversedLineOut : IO (String -> String) 
getReversedLineOut = reverse <$> getLine


readAndPut2 : IO ()
readAndPut2 = 
    do 
        s <- getLine 
        let t = s ++ s 
        putStrLn t



echoReversed'': IO()
echoReversed'' = 
    do 
        s <- getLine
        let t = reverse s 
        putStrLn "reversed:"
        putStrLn t
        putStrLn "original:"
        putStrLn s

echoReversed''': IO()
echoReversed'' = 
        getLine >>=  (\s => let t = reverse s in 
                                ?h)
        {- 
        s <- getLine
        let t = reverse s 
        putStrLn "reversed:"
        putStrLn t
        putStrLn "original:"
        putStrLn s
        -}
%hide Monad
%hide join

interface Applicative m=> Monad m where  
    -- (>>=) : m a -> ( a -> m b) -> m b 
    bind : m a -> (a -> m b) -> m b
    bind x f = let fx = join $ map f x
    join : m (m c) -> m c 
    -- infix operator f(a, b) == a f b 
    join x = x `bind'` id 


    -- convert things to
    -- a ~> m b
    -- b ~> c
    -- bind' : (a -> m b) -> (b -> m c) -> a -> m c


record User where
    constructor MkUser
    userId : Nat 
    name : String

passwd: List User
passwd = [MkUser 0 "root", MkUser 1 "Me"]

findUserNameById : Nat -> Reader (List User) (Maybe String)
--find : (a -> Bool) -> List a -> Maybe a

-- Monad reader

-- ask : (Reader s) r  

-- runReader : Reader s r -> r

findUserNameById id = 
    do 
        env <- ask
        -- let u = find (\u => u.userId == id) env 
        let mu = find ((==id) . userId) env 
        let mn = map name mu
        pure mn 

                