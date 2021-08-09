{-# LANGUAGE MonadComprehensions, TypeSynonymInstances, FlexibleInstances #-}
module Ref.Surreal where
import Control.Applicative (liftA2,(<|>),Alternative(..))
import Data.List (intercalate,unfoldr)
import Data.Ratio

-- | An element of 'No' is a surreal number
-- if it is a /section/, meaning 
-- all element of the left part are strictly less than all 
-- elements of the right part.
-- Other elements are called games. 
data No = Set No :|: Set No

-- | If we carry around the information 
-- which of the sets is finite and which is infinite, 
-- then we can do some optimizations. 
-- In particular, in the Applicative instance of [] 
-- the operator '<*>' disregards all but the first element of the second list 
-- if the first list is infinite. 
data Cardinality = Finite | Infinite deriving (Eq)
data Set a = Set Cardinality [a]
enumerate :: Set a -> [a]
enumerate (Set _ set) = set
-- | a 'mappend' that works on streams too
splice :: [a] -> [a] -> [a]
splice [] xs = xs
splice xs [] = xs
splice (x:xs) (y:ys) = x:y:splice xs ys
instance Functor Set where
    fmap f (Set card set) = Set card (fmap f set)
-- This probably violates the Applicative laws 
-- but we don't care as the violation is 
-- in the ordering of infinite lists
instance Applicative Set where
    pure = Set Finite . pure
    (Set card functions) <*> (Set Finite [] ) = Set Finite []
    (Set Finite [])      <*> (Set Infinite _) = Set Finite []
    (Set card functions) <*> (Set Finite set) = Set card (functions <*> set)
    (Set card    (f:fs)) <*> xs@(Set Infinite set) = let
        Set _ set' = (Set card fs) <*> xs
        in Set Infinite (splice (fmap f set) set')
-- This probably violates the Monad laws 
-- but we don't care as the violation is 
-- in the ordering of infinite lists
instance Monad Set where
    return = pure
    (Set Finite []) >>= _ = Set Finite []
    (Set card (x:xs)) >>= k = case k x of
        Set Finite   kx -> let Set card' set = Set card xs >>= k 
            in Set card'     (kx <|> set)
        Set Infinite kx -> let Set _     set = Set card xs >>= k 
            in Set Infinite (splice kx set)
instance Alternative Set where
    empty = Set Finite []
    (Set Finite xs) <|> (Set Finite ys) = Set Finite (xs <|> ys)
    (Set _ xs) <|> Set _ ys = Set Infinite (splice xs ys)
-- | needed for 'maximum' and 'all'
instance Foldable Set where
    foldMap f = foldMap f . enumerate
instance Ord No where
        x@(xL :|: xR) <= y@(yL :|: yR) = all (x<) yR && all (y>) xL
instance Eq No where
        x == y = x <= y && y <= x

-- | first 'No' from which all others are constructed.
no0 :: No
no0 = empty :|: empty

-- | partial inverse to 'fromInteger'
integer :: No -> Maybe Integer
integer x = posInt x <|> negInt x where
        posInt (Set Finite [] :|: Set Finite []) = Just 0
        posInt (Set Finite xL :|: Set Finite []) = fmap ((+) 1) (posInt (maximum xL))
        posInt _ = Nothing
        negInt (Set Finite [] :|: Set Finite []) = Just 0
        negInt (Set Finite [] :|: Set Finite xR) = fmap (flip (-) 1) (negInt (minimum xR))
        negInt _ = Nothing

-- | We can really show only dyadics
instance Show No where
        show x = case integer x of
                Just n -> show n
                Nothing -> let (Set cardL xL :|: Set cardR xR) = x in concat [
                        "{",
                        if cardL == Finite then intercalate "," (map show xL) else "infinite",
                        "}|{",
                        if cardR == Finite then intercalate "," (map show xR) else "infinite",
                        "}"
                        ]

-- | embedding of dyadic rationals into 'No'
fromDyadic :: Rational -> Maybe No
fromDyadic q = let 
    (x,y) = (numerator q,denominator q)
    (k,mod2x) = x `divMod` 2
    (n,mod2y) = y `divMod` 2
    in case (mod2x,mod2y) of
        (1,0) -> do
            l <- fromDyadic (k % n)
            r <- fromDyadic ((k+1) % n)
            return (pure l :|: pure r)
        (_,1) -> if n==0 
            then Just (fromInteger x) -- base case: an integer
            else Nothing -- not a dyadic rational
        (0,0) -> error "ratio not in minimal form"

-- | A Dedekind cut of the Rationals.
-- Vaguely speaking, 
-- 
-- @
-- 'integralBounds' x = ('floor' x,'ceiling' x)
-- 'compRational' x q = 'compare' x q
-- @
data AbstractReal = AbstractReal {
    integralBounds :: (Integer,Integer),
    compRational :: Rational -> Ordering
    }
class Abstract real where
    abstract :: real -> AbstractReal
instance Abstract Rational where
    abstract x = AbstractReal {
        integralBounds = (floor x,ceiling x),
        compRational = compare x
        }
instance Abstract Integer where
    abstract n = AbstractReal {
        integralBounds = (n,n),
        compRational = compare (fromInteger n)
        }
instance Abstract Double where
    abstract = abstract . toRational
-- | a formal square root
data Root r = Root Int r deriving (Show,Eq,Ord)
instance (Abstract r) => Abstract (Root r) where
    abstract (Root n r) = AbstractReal {
        integralBounds = babylonianRoot n (abstract r), 
        compRational = \q -> if q < 0 then GT else compRational (abstract r) (q^n)
        } 
-- TODO: data Pi = Pi
--       instance Abstract Pi

-- | Babylonian method computing the floor and ceiling of 
-- the n-th root (n > 0) in a binary search
babylonianRoot :: Int -> AbstractReal -> (Integer,Integer)
babylonianRoot n x = let
    -- invariant: lower^n <= x <= upper^n
    go (lower,upper) = if upper - lower <= 1
        then (lower,upper)
        else let mid = (lower+upper+1) `div` 2 in case compRational x (fromInteger (mid^n)) of
            LT -> go (lower,mid)
            EQ -> (mid,mid)
            _  -> go (mid,upper)
    in case integralBounds x of
        (0,0) -> (0,0) -- 0 is a fixed point of (^n)
        (0,1) -> (0,1) -- (^n) maps (0,1) to (0,1)
        (1,1) -> (1,1) -- 1 is a fixed point of (^n)
        (_,ceil) -> if ceil < 0 then error "no real root of negative numbers"
            else go (1,ceil)

-- Approximation of arbitrary, non-dyadic rationals. 
-- The result is a list of dyadic bounds, 
-- ascending in the first and descending in the second coordinate, 
-- which is empty if the input is an integer. 
-- If the list terminates with a pair (a,b) then 
-- the input x is the dyadic (a+b)/2. 
-- Otherwise the list is infinite and x was not dyadic. 
approxDyadic :: AbstractReal -> [(Rational,Rational)]
approxDyadic x = let
    (fl,ce) = integralBounds x
    go :: (Integer,Integer) -> Maybe (Integer,Integer)
    -- Preconditions: ((k+1)/n) > x > (k/n)
    --                n is a power of two
    go (k,n) = let    
        k' = 2*k+1
        n' = 2*n
        in case compRational x (k' % n') of
            LT -> Just (2*k,n') -- (2*k+1)/(2*n) > x > (2*k)/(2*n)
            GT -> Just (k' ,n')
            EQ -> Nothing -- terminate recusion
    in if fl == ce
        then [] -- x is an integer
        else unfoldr (\left -> fmap (\newleft@(k,n) -> ((k%n,(k+1) % n),newleft)) (go left)) (fl,1)

-- | 'fromRational' for 'No'
rationalNo :: Rational -> No
rationalNo x = case fromDyadic x of
    Just no -> no -- this could be handled by approxDyadic,
    -- but we do not know a priori whether approxDyadic returns a finite list or a stream. 
    Nothing -> case approxDyadic (abstract x) of
        [] -> error "fromDyadic returned Nothing on an integer"
        dyadics@(_:_) -> let 
            (xL,xR) = unzip dyadics
            dyadicRational = maybe (error "approxDyadic returned a non-dyadic") id . fromDyadic
            in Set Infinite (fmap dyadicRational xL) :|: Set Infinite (fmap dyadicRational xR)

-- | first infinite ordinal
omega :: No
omega = Set Infinite nats :|: empty where
        nats = map fromInteger [1..]

-- * Optimizations

-- We know in finite xL only a maximum element is relevant
-- and likewise in xR only a minimal element. 
-- Can we do better for infinite sets?
-- for 'addNo' it is obvious that each of the four 
-- sub-sets remains sorted, whence we could make a monotone splice.
maximumSet :: Ord a => Set a -> Set a
maximumSet (Set Finite []) = empty
maximumSet (Set Finite set) = pure (maximum set)
maximumSet set@(Set Infinite _) = set
minimumSet :: Ord a => Set a -> Set a
minimumSet (Set Finite []) = empty
minimumSet (Set Finite set) = pure (minimum set)
minimumSet set@(Set Infinite _) = set 
-- TODO: Simplify within each equivalence class.
-- e.g. {-1}|{1} == 0


-- * Arithmetic


-- | copied almost verbatim from /On numbers and games/
negateNo :: No -> No
negateNo (xL :|: xR) = fmap negateNo xR :|: fmap negateNo xL

-- | copied almost verbatim from /On numbers and games/
addNo :: No -> No -> No
addNo x@(xLs :|: xRs) y@(yLs :|: yRs) = maximumSet l :|: minimumSet r where
        l = [xL+y | xL <- xLs] <|> [x+yL | yL <- yLs]
        r = [xR+y | xR <- xRs] <|> [x+yR | yR <- yRs]

-- | copied almost verbatim from /On numbers and games/
multNo :: No -> No -> No
multNo x@(xLs :|: xRs) y@(yLs :|: yRs) = maximumSet l :|: minimumSet r where
        l = ll <|> lr
        r = rl <|> rr
        ll = [xL*y + x*yL - xL*yL | xL <- xLs, yL <- yLs]
        lr = [xR*y + x*yR - xR*yR | xR <- xRs, yR <- yRs]
        rl = [xL*y + x*yR - xL*yR | xL <- xLs, yR <- yRs]
        rr = [xR*y + x*yL - xR*yL | xR <- xRs, yL <- yLs]
-- without maximumSet and minimumSet,
-- 2*3 allocates 127751783504 Bytes of memory!
-- with maximumSet and minimumSet,
-- 2*3 allocates  19430710744 Bytes of memory.

instance Num No where
        fromInteger n = case compare n 0 of
            EQ -> no0
            GT -> pure (fromInteger (n-1)) :|: empty
            LT -> empty :|: pure (fromInteger (n+1))
        (+) = addNo
        negate = negateNo
        (*) = multNo
        signum x = case compare x no0 of
                EQ -> x
                LT -> fromInteger (-1)
                GT -> fromInteger 1
        abs x = if x < no0 then negateNo x else x
