-- https://blog.tmorris.net/posts/20-intermediate-haskell-exercises/

import Data.Char
import Data.Function

class Fluffy f where
    furry :: (a -> b) -> f a -> f b

instance Fluffy [] where
    furry = map

instance Fluffy Maybe where
    furry f (Just x) = Just (f x)
    furry f Nothing = Nothing

instance Fluffy ((->) t) where
    furry = (.)

newtype EitherLeft b a = EitherLeft (Either a b) deriving (Show, Eq)
newtype EitherRight a b = EitherRight (Either a b) deriving (Show, Eq)

instance Fluffy (EitherLeft t) where
    furry f (EitherLeft (Left x))= EitherLeft (Left (f x))
    furry f (EitherLeft (Right x))= EitherLeft (Right x)

instance Fluffy (EitherRight t) where
    furry f (EitherRight (Left x))= EitherRight (Left x)
    furry f (EitherRight (Right x))= EitherRight (Right (f x))

class Misty m where
    banana :: (a -> m b) -> m a -> m b
    unicorn :: a -> m a
    furry' :: (a -> b) -> m a -> m b
    furry' f = banana (unicorn . f)

instance Misty [] where
    banana f xs = concat $ map f xs
    unicorn x = [x]

instance Misty Maybe where
    banana f (Just x) = f x
    banana f Nothing = Nothing
    unicorn x = Just x

instance Misty ((->) t) where
    banana f g = \s -> f (g s) s
    unicorn x = \_ -> x

instance Misty (EitherLeft t) where
    banana f (EitherLeft (Left x)) = f x
    banana f (EitherLeft (Right y)) = EitherLeft (Right y)
    unicorn x = EitherLeft (Left x)

instance Misty (EitherRight t) where
    banana f (EitherRight (Left y)) = EitherRight (Left y)
    banana f (EitherRight (Right x)) = f x
    unicorn x = EitherRight (Right x)

jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- order of bananas matters

appleRev :: (Misty m) => m a -> m (a -> b) -> m b
apple :: (Misty m) => m a -> m (a -> b) -> m b
-- the function is unpacked before the argument
-- this mirrors the natve behavior
appleRev mx mf = banana (\f -> banana (\x -> unicorn (f x)) mx) mf
-- the argument is unpacked before the function
apple mx mf = banana (\x -> banana (\f -> unicorn (f x)) mf) mx

moppyRev :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
-- each iteration, current result is unpacked before the accumulator
-- this mirrors the native behavior
moppyRev list f
    = foldr
        (\x acc ->
            banana (\r -> banana (\rs -> unicorn (r:rs)) acc) (f x))
        (unicorn [])
        list
-- each iteration, accumulator is unpacked before the current result
moppy list f
    = foldr
        (\x acc ->
            banana (\rs -> banana (\r -> unicorn (r:rs)) (f x)) acc)
        (unicorn [])
        list

sausageRev :: (Misty m) => [m a] -> m [a]
sausage :: (Misty m) => [m a] -> m [a]
sausageRev xs = moppyRev xs id
sausage xs = moppy xs id

banana2Rev :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2Rev f x y = appleRev y (furry' f x)
banana2 f x y = apple y (furry' f x)

banana3Rev :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3Rev f x y z = appleRev z (banana2Rev f x y)
banana3 f x y z = apple z (banana2 f x y)

banana4Rev :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4Rev f x y z t = appleRev t (banana3Rev f x y z)
banana4 f x y z t = apple t (banana3 f x y z)

newtype State s a = State {
    state :: (s -> (s, a))
}

instance Fluffy (State s) where
    furry f (State g) = State (\s ->
        let
            (s', x) = g s
        in (s', f x))

instance Misty (State s) where
    banana f (State g) = State (\s ->
        let
            (s', x) = g s
            State g' = f x
        in g' s')
    unicorn x = State (\s -> (s, x))

infixr 0 |||
(|||) :: (Eq a, Show a) => a -> a -> Maybe String
x ||| y = do
    if x == y
    then Nothing
    else Just (" //>\n" ++ show x ++ "\n    |||\n" ++ show y)

infixr 0 //>
(//>) :: String -> Maybe String -> IO ()
n //> Nothing = return ()
n //> Just s = putStrLn (n ++ s)

inset :: a -> [a] -> [a]
inset a bs = foldr (\x ys -> x:a:ys) [] bs

exec :: s -> State s r -> (s, r)
exec s st = state st s

mofumofu :: IO ()
mofumofu = do
    putStrLn "Fluffy []"
    " 1110" //> furry (+1) [1, 2, 3] ||| [2, 3, 4]
    " 1120" //> furry show [11, -5] ||| ["11", "-5"]
    putStrLn "Fluffy Maybe"
    " 1210" //> furry (+1) (Just 1) ||| Just 2
    " 1220" //> furry show (Just 11) ||| Just "11"
    " 1230" //> furry show (Nothing :: Maybe Int) ||| Nothing
    putStrLn "Fluffy ((->) t)"
    " 1310" //> 10 & furry (+1) (*2) ||| 21
    " 1320" //> 30 & furry (+1) (*2) ||| 61
    " 1330" //> 10 & furry show (*2) ||| "20"
    " 1340" //> (-5) & furry show (*2) ||| "-10"
    " 1350" //> 10 & furry read show ||| 10
    " 1360" //> 10 & furry (++"%") show ||| "10%"
    putStrLn "Fluffy (EitherLeft b a)"
    " 1410"
        //> furry (+1) (EitherLeft (Left 1 :: Either Int ()))
        ||| (EitherLeft (Left 2))
    " 1420"
        //> furry show (EitherLeft (Left 11 :: Either Int ()))
        ||| (EitherLeft (Left "11"))
    " 1430"
        //> furry (++"bar") (EitherLeft (Right 14 :: Either String Int))
        ||| (EitherLeft (Right 14))
    " 1440"
        //> furry (+1) (EitherLeft (Right "bar" :: Either Int String))
        ||| (EitherLeft (Right "bar"))
    putStrLn "Fluffy (EitherRight a b)"
    " 1510"
        //> furry (+1) (EitherRight (Right 1 :: Either () Int))
        ||| (EitherRight (Right 2))
    " 1520"
        //> furry show (EitherRight (Right 11 :: Either () Int))
        ||| (EitherRight (Right "11"))
    " 1530"
        //> furry (++"bar") (EitherRight (Left 14 :: Either Int String))
        ||| (EitherRight (Left 14))
    " 1540"
        //> furry (+1) (EitherRight (Left "bar" :: Either String Int))
        ||| (EitherRight (Left "bar"))

    putStrLn "Misty []"
    " 2110" //> unicorn 11 ||| [11]
    " 2120" //> banana (\x -> [x-1,x,x+1]) [10, 20] ||| [9,10,11, 19,20,21]
    " 2120"
        //> banana (\x -> if even x then [10*x, 100*x] else []) [1..6]
        ||| [20,200, 40,400, 60,600]
    " 2130" //> furry' (+1) [1, 2, 3] ||| [2, 3, 4]
    " 2140" //> furry' show [11, -5] ||| ["11", "-5"]
    putStrLn "Misty Maybe"
    " 2210" //> unicorn 11 ||| Just 11
    " 2220"
        //> map
            (banana (\x -> if even x then Just x else Nothing))
            (map Just [1..6] ++ [Nothing])
        ||| [Nothing, Just 2, Nothing, Just 4, Nothing, Just 6, Nothing]
    " 2230" //> furry' (+1) (Just 1) ||| Just 2
    " 2240" //> furry' show (Just 11) ||| Just "11"
    " 2250" //> furry' show (Nothing :: Maybe Int) ||| Nothing
    putStrLn "Misty ((->) t)"
    " 2310" //> (unicorn 10) "foo" ||| 10
    " 2320" //> (banana (\x -> (*x)) (*10)) 20 ||| let s=20 in s*10*s
    " 2330" //> (furry' (+1) (*2)) 10 ||| 21
    " 2340" //> (furry' (+1) (*2)) 30 ||| 61
    " 2350" //> (furry' show (*2)) 10 ||| "20"
    " 2360" //> (furry' show (*2)) (-5) ||| "-10"
    " 2370" //> (furry' read show) 10 ||| 10
    " 2380" //> (furry' (++"%") show) 10 ||| "10%"
    putStrLn "Misty (EitherLeft t)"
    " 2410"
        //> (unicorn 10 :: EitherLeft () Int)
        ||| EitherLeft (Left 10)
    " 2420"
        //> map
            (banana
                (\x -> if even x
                    then EitherLeft (Left (10*x))
                    else EitherLeft (Right (show x))))
            (map (EitherLeft . Left) [1..6] ++ [EitherLeft (Right "egg")])
        ||| map EitherLeft [
            Right "1",
            Left 20,
            Right "3",
            Left 40,
            Right "5",
            Left 60,
            Right "egg"]
    " 2430"
        //> furry' (+1) (EitherLeft (Left 1 :: Either Int ()))
        ||| (EitherLeft (Left 2))
    " 2440"
        //> furry' show (EitherLeft (Left 11 :: Either Int ()))
        ||| (EitherLeft (Left "11"))
    " 2450"
        //> furry' (++"bar") (EitherLeft (Right 14 :: Either String Int))
        ||| (EitherLeft (Right 14))
    " 2460"
        //> furry' (+1) (EitherLeft (Right "bar" :: Either Int String))
        ||| (EitherLeft (Right "bar"))
    putStrLn "Misty (EitherRight t)"
    " 2510"
        //> (unicorn 10 :: EitherRight () Int)
        ||| EitherRight (Right 10)
    " 2520"
        //> map
            (banana
                (\x -> if even x
                    then EitherRight (Right (10*x))
                    else EitherRight (Left (show x))))
            (map (EitherRight . Right) [1..6] ++ [EitherRight (Left "egg")])
        ||| map EitherRight [
            Left "1",
            Right 20,
            Left "3",
            Right 40,
            Left "5",
            Right 60,
            Left "egg"]
    " 2530"
        //> furry' (+1) (EitherRight (Right 1 :: Either () Int))
        ||| (EitherRight (Right 2))
    " 2540"
        //> furry' show (EitherRight (Right 11 :: Either () Int))
        ||| (EitherRight (Right "11"))
    " 2550"
        //> furry' (++"bar") (EitherRight (Left 14 :: Either Int String))
        ||| (EitherRight (Left 14))
    " 2560"
        //> furry' (+1) (EitherRight (Left "bar" :: Either String Int))
        ||| (EitherRight (Left "bar"))

    putStrLn "jellybean of []"
    " 3110" //> jellybean [[1, 2], [], [3]] ||| [1, 2, 3]
    putStrLn "jellybean of Maybe"
    " 3210" //> jellybean (Just (Just 1)) ||| Just 1
    " 3220" //> jellybean (Just Nothing :: Maybe (Maybe Int)) ||| Nothing
    " 3230" //> jellybean (Nothing :: Maybe (Maybe Int)) ||| Nothing
    putStrLn "jellybean of ((->) t)"
    " 3310" //> (jellybean (\x -> (\y -> x*y))) 9 ||| 81
    putStrLn "jellybean of (EitherLeft t)"
    " 3410"
        //> jellybean (EitherLeft (Left (EitherLeft (Left 10))))
        ||| (EitherLeft (Left 10) :: EitherLeft () Int)
    " 3420"
        //> jellybean (EitherLeft (Left (EitherLeft (Right 10))))
        ||| (EitherLeft (Right 10) :: EitherLeft Int ())
    " 3430"
        //> jellybean (EitherLeft (Right "foo"))
        ||| (EitherLeft (Right "foo") :: EitherLeft String ())
    putStrLn "jellybean of (EitherRight t)"
    " 3510"
        //> jellybean (EitherRight (Right (EitherRight (Right 10))))
        ||| (EitherRight (Right 10) :: EitherRight () Int)
    " 3520"
        //> jellybean (EitherRight (Right (EitherRight (Left 10))))
        ||| (EitherRight (Left 10) :: EitherRight Int ())
    " 3530"
        //> jellybean (EitherRight (Left "foo"))
        ||| (EitherRight (Left "foo") :: EitherRight String ())

    putStrLn "appleRev of []"
    " 4110"
        //> appleRev [100, 101, 102] [show, (\x -> [chr x])]
        ||| ["100", "101", "102", "d", "e", "f"]
    putStrLn "appleRev of Maybe"
    " 4210" //> appleRev (Just 1) (Just show) ||| Just "1"
    " 4220" //> appleRev (Nothing :: Maybe Int) (Just show) ||| Nothing
    " 4230" //> appleRev (Just 1) Nothing ||| (Nothing :: Maybe String)
    " 4240" //> appleRev Nothing Nothing ||| (Nothing :: Maybe String)
    putStrLn "appleRev of ((->) t)"
    " 4310" //> (appleRev (*10) (\x -> (*x))) 9 ||| 810
    putStrLn "appleRev of (EitherLeft t)"
    " 4410"
        //> appleRev (EitherLeft (Left 1)) (EitherLeft (Left (*2)))
        ||| (EitherLeft (Left 2) :: EitherLeft String Int)
    " 4420"
        //> appleRev (EitherLeft (Left 1)) (EitherLeft (Right "boo"))
        ||| (EitherLeft (Right "boo") :: EitherLeft String Int)
    " 4430"
        //> appleRev (EitherLeft (Right "spam")) (EitherLeft (Left (*2)))
        ||| (EitherLeft (Right "spam") :: EitherLeft String Int)
    " 4440"
        //> appleRev (EitherLeft (Right "spam")) (EitherLeft (Right "boo"))
        ||| (EitherLeft (Right "boo") :: EitherLeft String Int)
    putStrLn "appleRev of (EitherRight t)"
    " 4510"
        //> appleRev (EitherRight (Right 1)) (EitherRight (Right (*2)))
        ||| (EitherRight (Right 2) :: EitherRight String Int)
    " 4520"
        //> appleRev (EitherRight (Right 1)) (EitherRight (Left "boo"))
        ||| (EitherRight (Left "boo") :: EitherRight String Int)
    " 4530"
        //> appleRev (EitherRight (Left "spam")) (EitherRight (Right (*2)))
        ||| (EitherRight (Left "spam") :: EitherRight String Int)
    " 4540"
        //> appleRev (EitherRight (Left "spam")) (EitherRight (Left "boo"))
        ||| (EitherRight (Left "boo") :: EitherRight String Int)

    putStrLn "apple of []"
    " 5110"
        //> apple [100, 101, 102] [show, (\x -> [chr x])]
        ||| ["100", "d", "101", "e", "102", "f"]
    putStrLn "apple of Maybe"
    " 5210" //> apple (Just 1) (Just show) ||| Just "1"
    " 5220" //> apple (Nothing :: Maybe Int) (Just show) ||| Nothing
    " 5230" //> apple (Just 1) Nothing ||| (Nothing :: Maybe String)
    " 5240" //> apple Nothing Nothing ||| (Nothing :: Maybe String)
    putStrLn "apple of ((->) t)"
    " 5310" //> (apple (*10) (\x -> (*x))) 9 ||| 810
    putStrLn "apple of (EitherLeft t)"
    " 5410"
        //> apple (EitherLeft (Left 1)) (EitherLeft (Left (*2)))
        ||| (EitherLeft (Left 2) :: EitherLeft String Int)
    " 5420"
        //> apple (EitherLeft (Left 1)) (EitherLeft (Right "boo"))
        ||| (EitherLeft (Right "boo") :: EitherLeft String Int)
    " 5430"
        //> apple (EitherLeft (Right "spam")) (EitherLeft (Left (*2)))
        ||| (EitherLeft (Right "spam") :: EitherLeft String Int)
    " 5440"
        //> apple (EitherLeft (Right "spam")) (EitherLeft (Right "boo"))
        ||| (EitherLeft (Right "spam") :: EitherLeft String Int)
    putStrLn "apple of (EitherRight t)"
    " 5510"
        //> apple (EitherRight (Right 1)) (EitherRight (Right (*2)))
        ||| (EitherRight (Right 2) :: EitherRight String Int)
    " 5520"
        //> apple (EitherRight (Right 1)) (EitherRight (Left "boo"))
        ||| (EitherRight (Left "boo") :: EitherRight String Int)
    " 5530"
        //> apple (EitherRight (Left "spam")) (EitherRight (Right (*2)))
        ||| (EitherRight (Left "spam") :: EitherRight String Int)
    " 5540"
        //> apple (EitherRight (Left "spam")) (EitherRight (Left "boo"))
        ||| (EitherRight (Left "spam") :: EitherRight String Int)

    putStrLn "moppyRev of []"
    " 6110"
        //> moppyRev [100, 101] (\x -> [show x, [chr x]])
        ||| [["100", "101"], ["100", "e"], ["d", "101"], ["d", "e"]]
    putStrLn "moppyRev of Maybe"
    " 6210"
        //> moppyRev [100, 102] (\x -> if even x then Just x else Nothing)
        ||| Just [100, 102]
    " 6220"
        //> moppyRev [100, 101] (\x -> if even x then Just x else Nothing)
        ||| Nothing
    putStrLn "moppyRev of ((->) t)"
    " 6310"
        //> 10 & moppyRev [90, 91] (+)
        ||| [100, 101]
    putStrLn "moppyRev of (EitherLeft t)"
    " 6410"
        //> moppyRev
            [100, 102]
            (\x ->
                if even x
                    then EitherLeft (Left x)
                    else EitherLeft (Right (show x)))
        ||| EitherLeft (Left [100, 102])
    " 6420"
        //> moppyRev
            [100, 101, 103]
            (\x ->
                if even x
                    then EitherLeft (Left x)
                    else EitherLeft (Right (show x)))
        ||| EitherLeft (Right "101")
    putStrLn "moppyRev of (EitherRight t)"
    " 6510"
        //> moppyRev
            [100, 102]
            (\x ->
                if even x
                    then EitherRight (Right x)
                    else EitherRight (Left (show x)))
        ||| EitherRight (Right [100, 102])
    " 6520"
        //> moppyRev
            [100, 101, 103]
            (\x ->
                if even x
                    then EitherRight (Right x)
                    else EitherRight (Left (show x)))
        ||| EitherRight (Left "101")

    putStrLn "moppy of []"
    " 7110"
        //> moppy [100, 101] (\x -> [show x, [chr x]])
        ||| [["100", "101"], ["d", "101"], ["100", "e"], ["d", "e"]]
    putStrLn "moppy of Maybe"
    " 7210"
        //> moppy [100, 102] (\x -> if even x then Just x else Nothing)
        ||| Just [100, 102]
    " 7220"
        //> moppy [100, 101] (\x -> if even x then Just x else Nothing)
        ||| Nothing
    putStrLn "moppy of ((->) t)"
    " 7310"
        //> 10 & moppy [90, 91] (+)
        ||| [100, 101]
    putStrLn "moppy of (EitherLeft t)"
    " 7410"
        //> moppy
            [100, 102]
            (\x ->
                if even x
                    then EitherLeft (Left x)
                    else EitherLeft (Right (show x)))
        ||| EitherLeft (Left [100, 102])
    " 7420"
        //> moppy
            [100, 101, 103]
            (\x ->
                if even x
                    then EitherLeft (Left x)
                    else EitherLeft (Right (show x)))
        ||| EitherLeft (Right "103")
    putStrLn "moppy of (EitherRight t)"
    " 7510"
        //> moppy
            [100, 102]
            (\x ->
                if even x
                    then EitherRight (Right x)
                    else EitherRight (Left (show x)))
        ||| EitherRight (Right [100, 102])
    " 7520"
        //> moppy
            [100, 101, 103]
            (\x ->
                if even x
                    then EitherRight (Right x)
                    else EitherRight (Left (show x)))
        ||| EitherRight (Left "103")

    putStrLn "sausageRev of []"
    " 8110"
        //> sausageRev [[1], [2, 3], [4]]
        ||| [[1, 2, 4], [1, 3, 4]]
    putStrLn "sausageRev of Maybe"
    " 8210"
        //> sausageRev [Just 1, Just 2, Just 3]
        ||| Just [1, 2, 3]
    " 8220"
        //> sausageRev [Just 1, Nothing, Just 3]
        ||| Nothing
    putStrLn "sausageRev of ((->) t)"
    " 8310"
        //> 5 & sausageRev [(+10), (*2)]
        ||| [15, 10]
    putStrLn "sausageRev of (EitherLeft t)"
    " 8410"
        //> sausageRev (map (EitherLeft . Left) [1, 2, 3])
        ||| (EitherLeft (Left [1, 2, 3]) :: EitherLeft String [Int])
    " 8420"
        //> sausageRev
            (map (EitherLeft . Left) [1, 2, 3]
            ++ map (EitherLeft . Right) ["foo", "bar"])
        ||| EitherLeft (Right "foo")
    putStrLn "sausageRev of (EitherRight t)"
    " 8510"
        //> sausageRev (map (EitherRight . Right) [1, 2, 3])
        ||| (EitherRight (Right [1, 2, 3]) :: EitherRight String [Int])
    " 8520"
        //> sausageRev
            (map (EitherRight . Right) [1, 2, 3]
            ++ map (EitherRight . Left) ["foo", "bar"])
        ||| EitherRight (Left "foo")

    putStrLn "sausage of []"
    " 9110"
        //> sausage [[1], [2, 3], [4]]
        ||| [[1, 2, 4], [1, 3, 4]]
    putStrLn "sausage of Maybe"
    " 9210"
        //> sausage [Just 1, Just 2, Just 3]
        ||| Just [1, 2, 3]
    " 9220"
        //> sausage [Just 1, Nothing, Just 3]
        ||| Nothing
    putStrLn "sausage of ((->) t)"
    " 9310"
        //> 5 & sausage [(+10), (*2)]
        ||| [15, 10]
    putStrLn "sausage of (EitherLeft t)"
    " 9410"
        //> sausage (map (EitherLeft . Left) [1, 2, 3])
        ||| (EitherLeft (Left [1, 2, 3]) :: EitherLeft String [Int])
    " 9420"
        //> sausage
            (map (EitherLeft . Left) [1, 2, 3]
            ++ map (EitherLeft . Right) ["foo", "bar"])
        ||| EitherLeft (Right "bar")
    putStrLn "sausage of (EitherRight t)"
    " 9510"
        //> sausage (map (EitherRight . Right) [1, 2, 3])
        ||| (EitherRight (Right [1, 2, 3]) :: EitherRight String [Int])
    " 9520"
        //> sausage
            (map (EitherRight . Right) [1, 2, 3]
            ++ map (EitherRight . Left) ["foo", "bar"])
        ||| EitherRight (Left "bar")

    putStrLn "banana2Rev of []"
    "10110" //> banana2Rev (+) [1, 2, 3] [40, 50] ||| [41, 51, 42, 52, 43, 53]
    putStrLn "banana2Rev of Maybe"
    "10210" //> banana2Rev (+) (Just 1) (Just 2) ||| Just 3
    "10220" //> banana2Rev (+) (Just 1) Nothing ||| Nothing
    "10230" //> banana2Rev (+) Nothing (Just 2) ||| Nothing
    "10240" //> banana2Rev (+) Nothing Nothing ||| Nothing
    putStrLn "banana2Rev of ((->) t)"
    "10310" //> 100 & banana2Rev (++) show (\x -> [chr x]) ||| "100d"
    putStrLn "banana2Rev of (EitherLeft t)"
    "10410"
        //> banana2Rev (+) (EitherLeft (Left 1)) (EitherLeft (Left 2))
        ||| (EitherLeft (Left 3) :: EitherLeft String Int)
    "10420"
        //> banana2Rev (+) (EitherLeft (Left 1)) (EitherLeft (Right "bar"))
        ||| EitherLeft (Right "bar")
    "10430"
        //> banana2Rev (+) (EitherLeft (Right "foo")) (EitherLeft (Left 2))
        ||| EitherLeft (Right "foo")
    "10440"
        //> banana2Rev (+)
            (EitherLeft (Right "foo")) (EitherLeft (Right "bar"))
        ||| (EitherLeft (Right "foo") :: EitherLeft String Int)
    putStrLn "banana2Rev of (EitherLeft t)"
    "10510"
        //> banana2Rev (+) (EitherRight (Right 1)) (EitherRight (Right 2))
        ||| (EitherRight (Right 3) :: EitherRight String Int)
    "10520"
        //> banana2Rev (+) (EitherRight (Right 1)) (EitherRight (Left "bar"))
        ||| EitherRight (Left "bar")
    "10530"
        //> banana2Rev (+) (EitherRight (Left "foo")) (EitherRight (Right 2))
        ||| EitherRight (Left "foo")
    "10540"
        //> banana2Rev (+)
            (EitherRight (Left "foo")) (EitherRight (Left "bar"))
        ||| (EitherRight (Left "foo") :: EitherRight String Int)

    putStrLn "banana2 of []"
    "11110" //> banana2 (+) [1, 2, 3] [40, 50] ||| [41, 42, 43, 51, 52, 53]
    putStrLn "banana2 of Maybe"
    "11210" //> banana2 (+) (Just 1) (Just 2) ||| Just 3
    "11220" //> banana2 (+) (Just 1) Nothing ||| Nothing
    "11230" //> banana2 (+) Nothing (Just 2) ||| Nothing
    "11240" //> banana2 (+) Nothing Nothing ||| Nothing
    putStrLn "banana2 of ((->) t)"
    "11310" //> 100 & banana2 (++) show (\x -> [chr x]) ||| "100d"
    putStrLn "banana2 of (EitherLeft t)"
    "11410"
        //> banana2 (+) (EitherLeft (Left 1)) (EitherLeft (Left 2))
        ||| (EitherLeft (Left 3) :: EitherLeft String Int)
    "11420"
        //> banana2 (+) (EitherLeft (Left 1)) (EitherLeft (Right "bar"))
        ||| EitherLeft (Right "bar")
    "11430"
        //> banana2 (+) (EitherLeft (Right "foo")) (EitherLeft (Left 2))
        ||| EitherLeft (Right "foo")
    "11440"
        //> banana2 (+)
            (EitherLeft (Right "foo")) (EitherLeft (Right "bar"))
        ||| (EitherLeft (Right "bar") :: EitherLeft String Int)
    putStrLn "banana2 of (EitherLeft t)"
    "11510"
        //> banana2 (+) (EitherRight (Right 1)) (EitherRight (Right 2))
        ||| (EitherRight (Right 3) :: EitherRight String Int)
    "11520"
        //> banana2 (+) (EitherRight (Right 1)) (EitherRight (Left "bar"))
        ||| EitherRight (Left "bar")
    "11530"
        //> banana2 (+) (EitherRight (Left "foo")) (EitherRight (Right 2))
        ||| EitherRight (Left "foo")
    "11540"
        //> banana2 (+)
            (EitherRight (Left "foo")) (EitherRight (Left "bar"))
        ||| (EitherRight (Left "bar") :: EitherRight String Int)

    putStrLn "banana3Rev of []"
    "12110"
        //> banana3Rev (\x y z -> x ++ y ++ z)
            ["a", "b", "c"]
            ["-", "+"]
            ["d", "e"]
        ||| [
            "a-d", "a-e", "a+d", "a+e",
            "b-d", "b-e", "b+d", "b+e",
            "c-d", "c-e", "c+d", "c+e"]
    putStrLn "banana3Rev of Maybe"
    "12210"
        //> do
            a <- [Just "a", Nothing]
            b <- [Just "b", Nothing]
            c <- [Just "c", Nothing]
            [banana3Rev (\x y z -> x ++ y ++ z) a b c]
        ||| [Just "abc"] ++ replicate 7 Nothing
    putStrLn "banana3Rev of ((->) t)"
    "12310"
        //> 100 & banana3Rev
            (\x y z -> x ++ y ++ z)
            show
            (\x -> [chr x])
            (show . even)
        ||| "100dTrue"
    putStrLn "banana3Rev of (EitherLeft t)"
    "12410"
        //> do
            a <- [EitherLeft (Left 1), EitherLeft (Right "foo")]
            b <- [EitherLeft (Left 20), EitherLeft (Right "bar")]
            c <- [EitherLeft (Left 300), EitherLeft (Right "egg")]
            [banana3Rev (\x y z -> x + y + z) a b c]
        ||| [EitherLeft (Left 321)]
            ++ [EitherLeft (Right "egg")]
            ++ replicate 2 (EitherLeft (Right "bar"))
            ++ replicate 4 (EitherLeft (Right "foo"))
    putStrLn "banana3Rev of (EitherRight t)"
    "12510"
        //> do
            a <- [EitherRight (Right 1), EitherRight (Left "foo")]
            b <- [EitherRight (Right 20), EitherRight (Left "bar")]
            c <- [EitherRight (Right 300), EitherRight (Left "egg")]
            [banana3Rev (\x y z -> x + y + z) a b c]
        ||| [EitherRight (Right 321)]
            ++ [EitherRight (Left "egg")]
            ++ replicate 2 (EitherRight (Left "bar"))
            ++ replicate 4 (EitherRight (Left "foo"))

    putStrLn "banana3 of []"
    "13110"
        //> banana3 (\x y z -> x ++ y ++ z)
            ["a", "b", "c"]
            ["-", "+"]
            ["d", "e"]
        ||| [
            "a-d", "b-d", "c-d", "a+d", "b+d", "c+d",
            "a-e", "b-e", "c-e", "a+e", "b+e", "c+e"]
    putStrLn "banana3 of Maybe"
    "13210"
        //> do
            a <- [Just "a", Nothing]
            b <- [Just "b", Nothing]
            c <- [Just "c", Nothing]
            [banana3 (\x y z -> x ++ y ++ z) a b c]
        ||| [Just "abc"] ++ take 7 (repeat Nothing)
    putStrLn "banana3 of ((->) t)"
    "13310"
        //> 100 & banana3
            (\x y z -> x ++ y ++ z)
            show
            (\x -> [chr x])
            (show . even)
        ||| "100dTrue"
    putStrLn "banana3 of (EitherLeft t)"
    "13410"
        //> do
            a <- [EitherLeft (Left 1), EitherLeft (Right "foo")]
            b <- [EitherLeft (Left 20), EitherLeft (Right "bar")]
            c <- [EitherLeft (Left 300), EitherLeft (Right "egg")]
            [banana3 (\x y z -> x + y + z) a b c]
        ||| [EitherLeft (Left 321)]
            & inset (EitherLeft (Right "foo"))
            & inset (EitherLeft (Right "bar"))
            & inset (EitherLeft (Right "egg"))
    putStrLn "banana3 of (EitherRight t)"
    "13510"
        //> do
            a <- [EitherRight (Right 1), EitherRight (Left "foo")]
            b <- [EitherRight (Right 20), EitherRight (Left "bar")]
            c <- [EitherRight (Right 300), EitherRight (Left "egg")]
            [banana3 (\x y z -> x + y + z) a b c]
        ||| [EitherRight (Right 321)]
            & inset (EitherRight (Left "foo"))
            & inset (EitherRight (Left "bar"))
            & inset (EitherRight (Left "egg"))

    putStrLn "banana4Rev of []"
    "14110"
        //> banana4Rev (\x y z t -> x ++ y ++ z ++ t)
            ["a", "b", "c"]
            ["-", "+"]
            ["d", "e"]
            [".", "/"]
        ||| [
            "a-d.", "a-d/", "a-e.", "a-e/", "a+d.", "a+d/", "a+e.", "a+e/",
            "b-d.", "b-d/", "b-e.", "b-e/", "b+d.", "b+d/", "b+e.", "b+e/",
            "c-d.", "c-d/", "c-e.", "c-e/", "c+d.", "c+d/", "c+e.", "c+e/"]
    putStrLn "banana4Rev of Maybe"
    "14210"
        //> do
            a <- [Just "a", Nothing]
            b <- [Just "b", Nothing]
            c <- [Just "c", Nothing]
            d <- [Just "d", Nothing]
            [banana4Rev (\x y z t -> x ++ y ++ z ++ t) a b c d]
        ||| [Just "abcd"] ++ replicate 15 Nothing
    putStrLn "banana4Rev of ((->) t)"
    "14310"
        //> 100 & banana4Rev
            (\x y z t -> x ++ y ++ z ++ t)
            show
            (\x -> [chr x])
            (show . even)
            (show . odd)
        ||| "100dTrueFalse"
    putStrLn "banana4Rev of (EitherLeft t)"
    "14410"
        //> do
            a <- [EitherLeft (Left 1), EitherLeft (Right "foo")]
            b <- [EitherLeft (Left 20), EitherLeft (Right "bar")]
            c <- [EitherLeft (Left 300), EitherLeft (Right "egg")]
            d <- [EitherLeft (Left 4000), EitherLeft (Right "spam")]
            [banana4Rev (\x y z t -> x + y + z + t) a b c d]
        ||| [EitherLeft (Left 4321)]
            ++ [EitherLeft (Right "spam")]
            ++ replicate 2 (EitherLeft (Right "egg"))
            ++ replicate 4 (EitherLeft (Right "bar"))
            ++ replicate 8 (EitherLeft (Right "foo"))
    putStrLn "banana4Rev of (EitherRight t)"
    "14510"
        //> do
            a <- [EitherRight (Right 1), EitherRight (Left "foo")]
            b <- [EitherRight (Right 20), EitherRight (Left "bar")]
            c <- [EitherRight (Right 300), EitherRight (Left "egg")]
            d <- [EitherRight (Right 4000), EitherRight (Left "spam")]
            [banana4Rev (\x y z t -> x + y + z + t) a b c d]
        ||| [EitherRight (Right 4321)]
            ++ [EitherRight (Left "spam")]
            ++ replicate 2 (EitherRight (Left "egg"))
            ++ replicate 4 (EitherRight (Left "bar"))
            ++ replicate 8 (EitherRight (Left "foo"))

    putStrLn "banana4 of []"
    "15110"
        //> banana4 (\x y z t -> x ++ y ++ z ++ t)
            ["a", "b", "c"]
            ["-", "+"]
            ["d", "e"]
            [".", "/"]
        ||| [
            "a-d.", "b-d.", "c-d.", "a+d.", "b+d.", "c+d.",
            "a-e.", "b-e.", "c-e.", "a+e.", "b+e.", "c+e.",
            "a-d/", "b-d/", "c-d/", "a+d/", "b+d/", "c+d/",
            "a-e/", "b-e/", "c-e/", "a+e/", "b+e/", "c+e/"]
    putStrLn "banana4 of Maybe"
    "15210"
        //> do
            a <- [Just "a", Nothing]
            b <- [Just "b", Nothing]
            c <- [Just "c", Nothing]
            d <- [Just "d", Nothing]
            [banana4 (\x y z t -> x ++ y ++ z ++ t) a b c d]
        ||| [Just "abcd"] ++ take 15 (repeat Nothing)
    putStrLn "banana4 of ((->) t)"
    "15310"
        //> 100 & banana4
            (\x y z t -> x ++ y ++ z ++ t)
            show
            (\x -> [chr x])
            (show . even)
            (show . odd)
        ||| "100dTrueFalse"
    putStrLn "banana4 of (EitherLeft t)"
    "15410"
        //> do
            a <- [EitherLeft (Left 1), EitherLeft (Right "foo")]
            b <- [EitherLeft (Left 20), EitherLeft (Right "bar")]
            c <- [EitherLeft (Left 300), EitherLeft (Right "egg")]
            d <- [EitherLeft (Left 4000), EitherLeft (Right "spam")]
            [banana4 (\x y z t -> x + y + z + t) a b c d]
        ||| [EitherLeft (Left 4321)]
            & inset (EitherLeft (Right "foo"))
            & inset (EitherLeft (Right "bar"))
            & inset (EitherLeft (Right "egg"))
            & inset (EitherLeft (Right "spam"))
    putStrLn "banana4 of (EitherRight t)"
    "15510"
        //> do
            a <- [EitherRight (Right 1), EitherRight (Left "foo")]
            b <- [EitherRight (Right 20), EitherRight (Left "bar")]
            c <- [EitherRight (Right 300), EitherRight (Left "egg")]
            d <- [EitherRight (Right 4000), EitherRight (Left "spam")]
            [banana4 (\x y z t -> x + y + z + t) a b c d]
        ||| [EitherRight (Right 4321)]
            & inset (EitherRight (Left "foo"))
            & inset (EitherRight (Left "bar"))
            & inset (EitherRight (Left "egg"))
            & inset (EitherRight (Left "spam"))

    putStrLn "Furry (State s)"
    "16110"
        //> exec 5 (State (\s -> (s*10, show s)))
        ||| (50, "5")
    "16120"
        //> exec 5 (furry (++"%") (State (\s -> (s*10, show s))))
        ||| (50, "5%")
    putStrLn "Misty (State s)"
    "16130"
        //> exec 5 (unicorn "foo")
        ||| (5, "foo")
    "16140"
        //> exec 5
            (banana
                (\x -> State (\s -> (s+1, x ++ "," ++ show s)))
                (State (\s -> (s*10, show s))))
        ||| (51, "5,50")
    "16150"
        //> exec 5 (furry' (++"%") (State (\s -> (s*10, show s))))
        ||| (50, "5%")
    putStrLn "jellybean of (State s)"
    "16160"
        //> exec 5
            (jellybean
                (State (\s ->
                    (s + 200, State (\s' ->
                        (s'*5, show s ++ "," ++ show s'))))))
        ||| (205*5, "5,205")
    putStrLn "appleRev of (State s)"
    "16170"
        //> exec 5
            (appleRev
                (State (\s -> (s*10, show s)))
                (State (\s -> (s + 1, \x -> x ++ "," ++ show s))))
        ||| (60, "6,5")
    putStrLn "apple of (State s)"
    "16180"
        //> exec 5
            (apple
                (State (\s -> (s*10, show s)))
                (State (\s -> (s + 1, \x -> x ++ "," ++ show s))))
        ||| (51, "5,50")
    putStrLn "moppyRev of (State s)"
    "16190"
        //> exec ""
            (moppyRev
                ["a", "b", "c"]
                (\x ->
                    State (\s ->
                        (s ++ "|" ++ x, show (length s) ++ " | " ++ x))))
        ||| ("|a|b|c", ["0 | a", "2 | b", "4 | c"])
    putStrLn "moppy of (State s)"
    "16200"
        //> exec ""
            (moppy
                ["a", "b", "c"]
                (\x ->
                    State (\s ->
                        (s ++ "|" ++ x, show (length s) ++ " | " ++ x))))
        ||| ("|c|b|a", ["4 | a", "2 | b", "0 | c"])
    putStrLn "sausageRev of (State s)"
    "16210"
        //> exec ""
            (sausageRev [
                State (\s -> (s ++ "a", 10)),
                State (\s -> (s ++ "b", 20)),
                State (\s -> (s ++ "c", 30))])
        ||| ("abc", [10, 20, 30])
    putStrLn "sausage of (State s)"
    "16220"
        //> exec ""
            (sausage [
                State (\s -> (s ++ "a", 10)),
                State (\s -> (s ++ "b", 20)),
                State (\s -> (s ++ "c", 30))])
        ||| ("cba", [10, 20, 30])
    putStrLn "banana2Rev of (State s)"
    "16230"
        //> exec ""
            (banana2Rev
                (++)
                (State (\s -> (s ++ "a", [1, 2])))
                (State (\s -> (s ++ "b", [3]))))
        ||| ("ab", [1, 2, 3])
    putStrLn "banana2 of (State s)"
    "16240"
        //> exec ""
            (banana2
                (++)
                (State (\s -> (s ++ "a", [1, 2])))
                (State (\s -> (s ++ "b", [3]))))
        ||| ("ba", [1, 2, 3])
    putStrLn "banana3Rev of (State s)"
    "16250"
        //> exec ""
            (banana3Rev
                (\x y z -> x ++ y ++ z)
                (State (\s -> (s ++ "a", [1, 2])))
                (State (\s -> (s ++ "b", [3])))
                (State (\s -> (s ++ "c", [4, 5, 6]))))
        ||| ("abc", [1, 2, 3, 4, 5, 6])
    putStrLn "banana3 of (State s)"
    "16260"
        //> exec ""
            (banana3
                (\x y z -> x ++ y ++ z)
                (State (\s -> (s ++ "a", [1, 2])))
                (State (\s -> (s ++ "b", [3])))
                (State (\s -> (s ++ "c", [4, 5, 6]))))
        ||| ("cba", [1, 2, 3, 4, 5, 6])
    putStrLn "banana4Rev of (State s)"
    "16270"
        //> exec ""
            (banana4Rev
                (\x y z t -> x ++ y ++ z ++ t)
                (State (\s -> (s ++ "a", [1, 2])))
                (State (\s -> (s ++ "b", [])))
                (State (\s -> (s ++ "c", [4, 5, 6])))
                (State (\s -> (s ++ "d", [7]))))
        ||| ("abcd", [1, 2, 4, 5, 6, 7])
    putStrLn "banana4 of (State s)"
    "16280"
        //> exec ""
            (banana4
                (\x y z t -> x ++ y ++ z ++ t)
                (State (\s -> (s ++ "a", [1, 2])))
                (State (\s -> (s ++ "b", [])))
                (State (\s -> (s ++ "c", [4, 5, 6])))
                (State (\s -> (s ++ "d", [7]))))
        ||| ("dcba", [1, 2, 4, 5, 6, 7])

main = mofumofu
