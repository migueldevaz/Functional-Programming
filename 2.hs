import Data.Char

-- EXERCÍCIO 2

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2 * h : dobros t

numOcorre :: Char -> String -> Int
numOcorre _ "" = 0
numOcorre ch (h:t)
   | ch == h = 1 + numOcorre ch t
   | otherwise = numOcorre ch t

positivos :: [Int] -> Bool
positivos [] = False
positivos (h:[])
   | h > 0 = True
   | otherwise = False
positivos (h:t)
   | h > 0 = positivos t 
   | otherwise = False

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if (h > 0) then h : soPos t else soPos t

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t)
   | h < 0 = h + somaNeg t
   | otherwise = somaNeg t

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t)
   | length (h:t) > 3 = tresUlt t
   | otherwise = (h:t)

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t) = b : segundos t

nosPrimeiros :: Eq a => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros ch ((a,b):t)
   | ch == a = True
   | otherwise = nosPrimeiros ch t

-- EXERCÍCIO 4

type Polinomio = [Monomio]
type Monomio = (Float, Int)

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta gr ((c, e):t) = if (gr == e) then 1 + conta gr t else conta gr t

grau :: Polinomio -> Int
grau [] = 0
grau ((c, e): []) = e
grau ((c, e):(c1, e1):t)
   | e >= e1 = grau ((c, e):t)
   | otherwise = grau ((c1, e1):t)

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau exp ((c, e):t) = if (exp == e) then (c,e) : selgrau exp t else selgrau exp t
