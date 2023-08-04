module Main where

import Prelude hiding (
                        Functor,
                        fmap,
                        (<$>),
                        Semigroup, 
                        (<>), 
                        stimes, 
                        Monoid, 
                        mempty, 
                        mtimes, 
                        mconcat
                      )


data BTPar a = Leaf a | Node (BTPar a) (BTPar a) deriving Show

{- ExercÃ­cio 1 -}

allBips :: [a] -> [([a], [a])]
allBips xs = [(take n xs, drop n xs) | n <- [1..length xs - 1]]


{- ExercÃ­cio 2 -}
allTrees :: [a] -> [BTPar a]
allTrees [] = []
allTrees [x] = [Leaf x]
allTrees xs = [Node l r | (ys, zs) <- allBips xs, l <- allTrees ys, r <- allTrees zs]

class Semigroup a where
    (<>) :: a -> a -> a
    stimes :: Integral b => b -> a -> a
    stimes = fastTimes
    sconcat :: NonEmpty a -> a
    -- sconcat (x :| []) = x 
    -- sconcat (x :| (y:ys)) = x <> sconcat (y :| ys) 
    sconcat = fastCat
        
{- ExercÃ­cio 3 -}

naiveTimes :: (Semigroup a, Integral b) => b -> a -> a
naiveTimes l x = x
naiveTimes n x = x <> naiveTimes (n-1) x 


{- ExercÃ­cio 4 -}
fastTimes :: (Semigroup a, Integral b) => b -> a -> a
fastTimes 1 x = x
fastTimes n x
  | even n = y <> y
  | otherwise = x <> fastTimes (n - 1) x
  where
    y = fastTimes (n `div` 2 ) x


{- ExercÃ­cio 5 -}
data NonEmpty a = a :| [a] deriving Show
infixr 5 :|

class Functor f where
    fmap, (<$>) :: (a -> b) -> f a -> f b
    (<$>) = fmap

instance Functor [] where
    fmap = map

instance Functor NonEmpty where
    fmap f (a :| xs) = f a :| fmap f xs

-- data BTPar a = Leaf a | Node (BTPar a) (BTPar a) deriving Show    

instance Functor BTPar where
    fmap f (Leaf x) = Leaf $ f x
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

{- Exercicio 6: Na definiÃ§Ã£o de Semigroup -}

{- ExercÃ­cio 7 -}
pairing :: (a -> a -> a) -> [a] -> [a]
pairing f [] = []
pairing f [x] = [x]
pairing f (x0:x1:xs) = f x0 x1  : pairing f xs


{- ExercÃ­cio 8 -}

fastCat :: Semigroup a => NonEmpty a -> a
fastCat (x :| []) = x
fastCat (x :| xs) = fastCat (y :| ys)
    where
        (y:ys) = pairing (<>) (x:xs)

{- ExercÃ­cio 9 -}
newtype Sum a = Sum a


instance Num a => Semigroup (Sum a) where
    (Sum x) <> (Sum y) = Sum $ x + y

newtype Prod a = Prod a

instance Num a => Semigroup (Prod a) where
    (Prod x) <> (Prod y) = Prod $ x * y


{- ExercÃ­cio 10 -}
instance Semigroup [a] where
    (<>) = (++)

instance Semigroup (NonEmpty a) where
    (x :| xs) <> (y :| ys) = x :| xs ++ (y : ys)



{- ExercÃ­cio 11 -}
class Semigroup a => Monoid a where
    mempty :: a

    mappend :: a -> a -> a
    mappend = (<>)

    mtimes :: Integral b => b -> a -> a
    mtimes 0 _ =  mempty
    mtimes n x = stimes n x

    mconcat :: [a] -> a
    mconcat [] = mempty
    mconcat (x:xs) = sconcat (x :| xs)

{- ExercÃ­cio 12 -}

instance Num a => Monoid (Sum a) where
    mempty = 0

instance Num a => Monoid (Prod a) where
    mempty = 1

instance Monoid [] where
    mempty = []

{- Exercicio 13 -}

{- ExercÃ­cio 14 -}
newtype Sorted a = Sorted [a] deriving Show

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

instance Ord a => Semigroup (Sorted a) where
    (Sorted xs) <> (Sorted ys) = Sorted $ merge xs ys

instance Monoid (Sorted a) where
    mempty = Sorted []
    
{- ExercÃ­cio 15 -}
mergeSort :: Ord a => [a] -> [a]
mergeSort xs = ys
    where 
    (Sorted ys) = mconcat [Sorted [x] | x <- xs]


{- ExercÃ­cio 16 -}
newtype Fib = Fib [[Integer]]

instance Semigroup Fib where
    (Fib [[p11, p12], [p21, p22]]) <> (Fib [[q11, q12], [q21, q22]]) = 
        Fib [[r11, r12], [r21, r22]]
        where
            r11 = p11 * q11 + p12 * q21
            r12 = p11 * q12 + p12 * q22
            r21 = p21 * q11 + p22 * q21
            r22 = p21 * q12 + p22 * q22

fastFib :: Integer a => Int a
fastFib n = fb
    where
        (Fib [[_, _], [_, fb]]) = 
            stimes (n + 1) (Fib [[1,1], [1,0]])


main :: IO ()
main = do
    print $ allTrees [1,2,3,4]
    print $ mergeSort [1, 4, 5, 10, 2, 3, 6, 7, 9, 8]
    -- print $ [fastFib n | n <- [0..20]]
