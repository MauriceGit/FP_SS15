module Simple
where

-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 
fib     :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib a = fib (a - 1) + fib (a - 2)



-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 mit linearer Laufzeit

fib2    :: Integer -> Integer
fib2 0 = 0
fib2 1 = 1
fib2 a = fib2help (a-2) 0 1 

fib2help :: Integer -> Integer -> Integer -> Integer
fib2help 0 a b = (a+b)
fib2help c a b = fib2help (c-1) b (a+b)

-- Definieren Sie eine Funktion c (für Collatz), die berechnet
-- wie viele Rekursionsschritte benötigt werden, um
-- eine natürliche Zahl n >= 1 auf 1 zu
-- reduzieren.
--
-- Folgende Reduktionsregel sind dabei anzuwenden: Wenn n gerade ist,
-- so wird n halbiert, wenn n ungerade ist, so wird n verdreifacht und um
-- 1 erhöht.
    
c       :: Integer -> Integer
c 1 = 0
c n
    | (mod n 2) == 0 = c (div n 2) + 1
    | otherwise = c (n*3+1) + 1


-- Definieren Sie ein endrekurive Variante von c
    
c1      :: Integer -> Integer
c1 a = c1h a 0

c1h :: Integer -> Integer -> Integer
c1h 1 c = c
c1h a c = c1h (ch a) (c+1)
    where
    ch tmp
        | even tmp = (div tmp 2)
        | otherwise = a * 3 + 1
    
    
-- Definieren Sie eine Funktion cmax, die für ein
-- Intervall von Zahlen das Maximum der
-- Collatz-Funktion berechnet. Nutzen Sie die
-- vordefinierten Funkt min und max.

cmax    :: Integer -> Integer -> Integer
cmax lb ub = cmaxh lb ub lb

cmaxh   :: Integer -> Integer -> Integer -> Integer
cmaxh lb ub cb 
    | ub == cb = c ub
    | otherwise = max (c cb) (cmaxh lb ub (cb+1)) 


-- Definieren Sie eine Funktion imax, die für ein
-- Intervall von Zahlen das Maximum einer
-- ganzzahligen Funktion berechnet. Formulieren
-- Sie die obige Funktion cmax so um, dass sie mit imax arbeitet.

imax    :: (Integer -> Integer) -> Integer -> Integer -> Integer
imax f lb ub = imaxh f lb ub lb

imaxh   :: (Integer -> Integer) -> Integer -> Integer -> Integer -> Integer
imaxh f lb ub cb = snd (imaxh2 f lb ub cb)


cmax1   :: Integer -> Integer -> Integer
cmax1 = imax c

-- Entwickeln Sie eine Funktion,
-- die die Position und den Wert bestimmt, an der
-- das Maximum angenommen wird.
-- Versuchen Sie, eine endrekursive Lösung zu finden
-- (mit einer lokalen Hilfsfunktion).

imax0 :: Integer -> Integer -> (Integer, Integer)
imax0 = imax2 c 

imax2   :: (Integer -> Integer) -> Integer -> Integer -> (Integer, Integer)
imax2 f lb ub = imaxh2 f lb ub lb

imaxh2   :: (Integer -> Integer) -> Integer -> Integer -> Integer -> (Integer, Integer)
imaxh2 f lb ub cb 
    | cb == ub = (cb, res)
    | otherwise = if y > res then (x, y) else (cb, res)
    where
        (x, y) = imaxh2 f lb ub (cb+1)
        res = f cb
        
    
    

