import Data.Function.Memoize
import Data.Ratio
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

-- Stieltjes

twine :: [a] -> [a] -> [a]
twine [] ys = ys
twine (x:xs) ys = x : twine ys xs

stieltjes :: Fractional a => Vector a -> Vector a
stieltjes cs =
    if V.null cs then
        V.empty
    else
        V.fromList (cs ! 0 : twine qs es)
  where
    n = length cs `div` 2
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
