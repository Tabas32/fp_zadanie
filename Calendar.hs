module Calendar where

import Data.List (zip4)
import System.IO

{- Tento modul obsahuje:
	# funkcie pre generovanie kalendara

	# funkcie pre zobrazovanie casti kalendara

	# funkcie pre skladanie jednotlivych casti kalendara do jedneho obrazka
-}
main = do
    putStrLn (calendar 2019)

daynames = ["Ned", "Pon", "Uto", "Str", "Štv", "Pia", "Sob"]
mnames   = ["JANUÁR","FEBRUÁR","MAREC","APRÍL",
            "MÁJ","JÚN","JÚL","AUGUST",
            "SEPTEMBER","OKTÓBER","NOVEMBER","DECEMBER"]


-- Skladanie obrazka --------------------------------------------
height p = length p
width p  = length (head p)

above p q   = p ++ q
beside p q  = zipWith (++) p q

stack   = foldr1 above
spread  = foldr1 beside

empty (h,w) = replicate h (replicate w ' ')
block n     = stack . map spread . group n
group n xs  = [take n (drop j xs) | j <- [0,n..((length xs)-1)]]
blockT n    = spread . map stack . group n

lframe (m,n) p = (p `beside` empty (h, n-w))
                    `above` empty (m-h, n)
                 where h = height p
                       w = width p

display = unlines


-- Zobrazovanie ------------------------------------------------

calendar = display . block 3 . map picture . months

picture (mn, yr, fd, ml) = (title mn yr) `above` (table fd ml)

title mn yr   = lframe (2,25) [mn ++ " " ++ show yr]
table fd ml   = lframe (8,25) (daynames `beside` (entries fd ml))
entries fd ml = blockT 7 (dates fd ml)
dates fd ml   = map (date ml) [(1-fd) .. (42-fd)]
date ml d
    | (d < 1 || ml < d) = [rjustify 3 " "]
    | otherwise         = [rjustify 3 (show d)]

rjustify n s
    | (length s) < n  = (replicate (n - (length s)) ' ' ++ s)
    | otherwise = s


-- Skladanie struktury kalendara --------------------------------------
months yr = zip4 mnames (replicate 12 yr) (fstdays yr) (mlengths yr)

mlengths yr = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        where feb
                | (leap yr) = 29
                | otherwise = 28

leap yr
    | (yr `mod` 100 == 0) = (yr `mod` 400 == 0)
    | otherwise = (yr `mod` 4 == 0)

jan1 yr = (yr + (yr-1) `div` 4 - (yr-1) `div` 100 + (yr-1) `div` 400) `mod` 7
fstdays yr = take 12 (map (`mod` 7) (scanl (+) (jan1 yr) (mlengths yr)))


-- -------------------------------------------------------------------

-- ===================================================================
-- End of module
-- ===================================================================
