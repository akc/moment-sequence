import Data.Function.Memoize
import Data.Ratio
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

twine :: [a] -> [a] -> [a]
twine [] ys = ys
twine (x:xs) ys = x : twine ys xs

hankel :: Fractional a => Vector a -> Vector a
hankel = V.scanl (*) 1 . V.scanl1 (*) . snd . jacobi

-- J-fraction
jacobi :: Fractional a => Vector a -> (Vector a, Vector a)
jacobi cs =
    if V.null cs then
        (V.empty, V.empty)
    else
        (u, V.cons (cs ! 0) v)
  where
    ds = V.drop 1 $ stieltjes cs
    n = V.length ds `div` 2
    f (-1) = 0
    f i = ds ! i
    u = V.fromList $ (\k -> f (2*k-1) + f (2*k)) <$> [0 .. n]
    v = V.fromList $ (\k -> f (2*k) * f (2*k+1)) <$> [0 .. n - 1]

-- S-fraction
stieltjes :: Fractional a => Vector a -> Vector a
stieltjes cs =
    if V.null cs then
        V.empty
    else
        V.fromList (cs ! 0 : twine qs es)
  where
    n = V.length cs `div` 2
    qs = map (q 0) [ 1 .. n ]
    es = map (e 0) [ 1 .. n - 1 ]

    qM = memoize2 q
    eM = memoize2 e

    e _ 0 = 0
    e k j = eM (k+1) (j-1) + qM (k+1) j - qM k j

    q k 1 = cs ! (k+1) / cs ! k
    q k j = qM (k+1) (j-1) * eM (k+1) (j-1) / eM k (j-1)

cat :: Vector Rational
cat = V.fromList
      $ map fromRational [
             1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900
            ,2674440,9694845,35357670,129644790,477638700,1767263190
            ,6564120420,24466267020,91482563640,343059613650
            ,1289904147324,4861946401452,18367353072152,69533550916004
            ,263747951750360,1002242216651368,3814986502092304
            ]

euler :: Vector Rational
euler = V.fromList
      $ map fromRational [
             1,1,5,61,1385,50521,2702765,199360981,19391512145,2404879675441
            ,370371188237525,69348874393137901,15514534163557086905
            ,4087072509293123892361,1252259641403629865468285
            ,441543893249023104553682821,177519391579539289436664789665
            ]
