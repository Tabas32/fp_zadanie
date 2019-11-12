module Calendar where

import Data.List (zip4)
import System.IO

{- Tento modul obsahuje:
	# funkcie pre generovanie kalendara
        * months   ... generovanie kalendara
        * mlengths ... generovanie dlzky mesiacov
        * leap     ... zistenie ci je rok priestupny
        * jan1     ... vypocet dna prveho januara v danom roku
        * fstdays  ... vypocet prveho dna pre kazdy mesiac

	# funkcie pre zobrazovanie casti kalendara
        * calendar ... vykreslenie kalendara
        * picture  ... zobrazenie mesiaca s titulkom
        * title    ... zobrazenie nazvu a roku mesiaca
        * table    ... zobrazenie dni mesiaca s nazvami dni
        * entries  ... skladanie dni do tabulky dni mesiaca
        * dates    ... zobrazenie kazdeho dna v mesiaci
        * date     ... zobrazenie 1 dna
        * rjustify ... uprava retazca na danu dlzku

	# funkcie pre skladanie jednotlivych casti kalendara do jedneho obrazka
        * height  ... vyska obrazku
        * width   ... sirka obrazku
        * above   ... skladanie 2 obrazkov zvislo
        * beside  ... skladanie 2 obrazkov vodorovne
        * stack   ... skladanie obrazkov zvislo
        * spread  ... skladanie obrazkov vodorovne
        * empty   ... prazdny obrazok
        * block   ... zlozenie listu obrazkov do bloku danej sirky
        * blockT  ... transponovany blok obrazkov
        * group   ... uprav list na maticu listov danej sirky
        * lframe  ... oramovanie obrazku (obrazok je vlavo hore)
        * display ... pripravenie obrazka na vykreslenie

    # pouzitie funkcii
-}

main = do
    putStrLn (calendar 2019)


-- ===================================================================
-- Skladanie obrazka -------------------------------------------------

{- height ------------------------------------------------------------
# vyska obrazku

height :: Foldable t => t a -> Int
height p ... vracia dlzku p
------------------------------------------------------------------- -}
height p = length p
{- width -------------------------------------------------------------
# sirka obrazku

width :: Foldable t => [t a] -> Int
width p ... vracia dlzku prveho prvku p
------------------------------------------------------------------- -}
width p  = length (head p)

{- above -------------------------------------------------------------
# skladanie 2 obrazkov zvislo

above :: [a] -> [a] -> [a]
above p q ... vracia list tvoreny spojenim vstupnych listov p q
------------------------------------------------------------------- -}
above p q   = p ++ q
{- beside ------------------------------------------------------------
# skladanie 2 obrazkov vodorovne

beside :: [[a]] -> [[a]] -> [[a]]
beside p q ... vracia list tvoreny spojenim kazdeho elementu listov p q
------------------------------------------------------------------- -}
beside p q  = zipWith (++) p q


{- stack -------------------------------------------------------------
# skladanie obrazkov zvislo

stack :: [[a]] -> [a]
stack a ... spojenie prvkov listu a pomocou above 
------------------------------------------------------------------- -}
stack   = foldr1 above
{- spread ------------------------------------------------------------
# skladanie obrazkov vodorovne

spread :: [[[a]]] -> [[a]]
spread a ... spojenie prvkov list a pomocou beside
------------------------------------------------------------------- -}
spread  = foldr1 beside

{- empty -------------------------------------------------------------
# prazdny obrazok

empty :: (Int, Int) -> [[Char]]
empty (h,w) ... vracia list dlzky h obsahujuci prazdne retazce dlzky w
------------------------------------------------------------------- -}
empty (h,w) = replicate h (replicate w ' ')
{- block -------------------------------------------------------------
# zlozenie listu obrazkov do bloku danej sirky

block :: Int -> [[[a]]] -> [[a]]
Vstup:
    n  ... sirka bloku
    xs ... list obrazkov rovnakej sirky a vysky
block n xs ... spaja list xs do bloku s n stlpcami
           ... spojenie obrazkov p1..p12 pomocou block 3 ma za vysledok
                p1  p2  p3
                p4  p5  p6
                p7  p8  p9
                p10 p11 p12
------------------------------------------------------------------- -}
block n     = stack . map spread . group n
{- group  ------------------------------------------------------------
# uprav list na maticu listov danej sirky

group :: Int -> [a] -> [[a]]
group n xs ... rozdeli prvky xs do n-tic a vrati ako list
------------------------------------------------------------------- -}
group n xs  = [take n (drop j xs) | j <- [0,n..((length xs)-1)]]
{- blockT ------------------------------------------------------------
# transponovany blok obrazkov

blockT :: Int -> [[[a]]] -> [[a]]
Vstup:
    n  ... vyska bloku
    xs ... list obrazkov rovnakej sirky a vysky
blockT n xs ... spaja list xs do bloku s n riadkami
            ... spojenie obrazkov p1..p12 pomocou blockT 3 ma za vysledok
                p1  p4  p7 p10
                p2  p5  p8 p11
                p3  p6  p9 p12
------------------------------------------------------------------- -}
blockT n    = spread . map stack . group n

{- lframe ------------------------------------------------------------
# oramovanie obrazku (obrazok je vlavo hore)

lframe :: (Int, Int) -> [[Char]] -> [[Char]]
lframe (m,n) p ... oramuje obrazok p do vacsieho obrazku vysky m a sirky n
------------------------------------------------------------------- -}
lframe (m,n) p = (p `beside` empty (h, n-w))
                    `above` empty (m-h, n)
                 where h = height p
                       w = width p

{- display -----------------------------------------------------------
# pripravenie obrazka na vykreslenie

display :: [String] -> String
display xs ... vrati retazec tvoreny prvkamy listu xs ukoncenymi '\n'
------------------------------------------------------------------- -}
display = unlines


-- ===================================================================
-- Zobrazovanie ------------------------------------------------------

daynames = ["Ned", "Pon", "Uto", "Str", "Štv", "Pia", "Sob"]

{- calendar ----------------------------------------------------------
# vykreslenie kalendara

calendar :: Integer -> String
calendar n ... vrati kalendar pre rok n ako retazec pripraveny na vypis
------------------------------------------------------------------- -}
calendar = display . block 3 . map picture . months

{- picture -----------------------------------------------------------
# zobrazenie mesiaca s titulkom

picture :: (Ord a1, Num a1, Show a2, Show a1, Enum a1) => ([Char], a2, a1, a1) -> [[Char]]
picture (mn, yr, fd, ml) ... vracia obrazok mesiaca s nazvom mn,
                                                    v roku yr,
                                                    s prvym dnom fd,
                                                    a poctom dni ml
------------------------------------------------------------------- -}
picture (mn, yr, fd, ml) = (title mn yr) `above` (table fd ml)

{- title -------------------------------------------------------------
# zobrazenie nazvu a roku mesiaca

title :: Show a => [Char] -> a -> [[Char]]
title mn yr ... oramuje nazov mn a rok yr do obrazka 2x25
------------------------------------------------------------------- -}
title mn yr   = lframe (2,25) [mn ++ " " ++ show yr]
{- table -------------------------------------------------------------
# zobrazenie dni mesiaca s nazvami dni

table :: (Ord a, Num a, Show a, Enum a) => a -> a -> [[Char]]
table fd ml ... vracia obrazok s nazvami dni (daynames) a tabulkov dni 
                s rozmermi 8x25
------------------------------------------------------------------- -}
table fd ml   = lframe (8,25) (daynames `beside` (entries fd ml))
{- entries -----------------------------------------------------------
# skladanie dni do tabulky dni mesiaca

entries :: (Ord a, Num a, Show a, Enum a) => a -> a -> [[Char]]
entries fd ml ... vracia obrazok tabulky dni pre mesiac s prvim dnom fd
                  a poctom dni ml
------------------------------------------------------------------- -}
entries fd ml = blockT 7 (dates fd ml)
{- dates -------------------------------------------------------------
# zobrazenie kazdeho dna v mesiaci

dates :: (Ord a, Num a, Show a, Enum a) => a -> a -> [[[Char]]]
dates fd ml ... vracia list dni pre mesiac s prvim dnom fd a poctom dni
                ml, kde kazdy den je retazec dlzky 3 a obsahuje cislo
                dna v mesiaci
------------------------------------------------------------------- -}
dates fd ml   = map (date ml) [(1-fd) .. (42-fd)]
{- date --------------------------------------------------------------
# zobrazenie 1 dna

date :: (Ord a, Num a, Show a) => a -> a -> [[Char]]
date ml d ... vracia list s retazcom reprezentujucim den d v mesiaci
              dlzky ml, alebo prazdny retazec dlzky 3 ak den d nepatri
              do daneho pesiaca (rozumej d je < 0 alebo > ml)
------------------------------------------------------------------- -}
date ml d
    | (d < 1 || ml < d) = [rjustify 3 " "]
    | otherwise         = [rjustify 3 (show d)]

{- rjustify ----------------------------------------------------------
# uprava retazca na danu dlzku

rjustify :: Int -> [Char] -> [Char]
rjustify n s ... vlozi znak medzeri pred retazec s v takom mnozstve, aby
                 vysledny retazec mal dlzku n
------------------------------------------------------------------- -}
rjustify n s
    | (length s) < n  = (replicate (n - (length s)) ' ' ++ s)
    | otherwise = s


-- ===================================================================
-- Skladanie struktury kalendara -------------------------------------

mnames   = ["JANUÁR","FEBRUÁR","MAREC","APRÍL",
            "MÁJ","JÚN","JÚL","AUGUST",
            "SEPTEMBER","OKTÓBER","NOVEMBER","DECEMBER"]

{- months ------------------------------------------------------------
# generovanie kalendara

months :: (Integral a, Num d) => a -> [([Char], a, a, d)]
months yr ... vracia list mesiacov pre rok yr, kde kazdy mesiac je 
              stvorica (nazov mesiaca, rok, prvy den mesiaca, pocet dni mesiaca)
------------------------------------------------------------------- -}
months yr = zip4 mnames (replicate 12 yr) (fstdays yr) (mlengths yr)

{- mlengths ----------------------------------------------------------
# generovanie dlzky mesiacov

mlengths :: (Integral a2, Num a1) => a2 -> [a1]
mlengths yr ... vracia list s dlzkami jednotlivych mesiacov v roku yr
------------------------------------------------------------------- -}
mlengths yr = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        where feb
                | (leap yr) = 29
                | otherwise = 28

{- leap --------------------------------------------------------------
# zistenie ci je rok priestupny

leap :: integral a => a -> bool
leap yr ... vracia pravdivostnu hodnotu ci je rok yr priestupny
------------------------------------------------------------------- -}
leap yr
    | (yr `mod` 100 == 0) = (yr `mod` 400 == 0)
    | otherwise = (yr `mod` 4 == 0)

{- jan1 --------------------------------------------------------------
# vypocet dna prveho januara v danom roku

jan1 :: Integral a => a -> a
jan1 yr ... vracia cislo dna prveho januara v roku yr
------------------------------------------------------------------- -}
jan1 yr = (yr + (yr-1) `div` 4 - (yr-1) `div` 100 + (yr-1) `div` 400) `mod` 7
{- fstdays -----------------------------------------------------------
# vypocet prveho dna pre kazdy mesiac

fstdays :: Integral a => a -> [a]
fstdays yr ... vracia list cisel prvych dni mesiacov v roku yr
------------------------------------------------------------------- -}
fstdays yr = take 12 (map (`mod` 7) (scanl (+) (jan1 yr) (mlengths yr)))

-- -------------------------------------------------------------------
-- ===================================================================
{- Aplikacie funkcii -------------------------------------------------
> months 2019
[("JANU\193R",2019,2,31),("FEBRU\193R",2019,5,28),("MAREC",2019,5,31),("APR\205L",2019,1,30),("M\193J",2019,3,31),("J\218N",2019,6,30),("J\218L",2019,1,31),("AUGUST",2019,4,31),("SEPTEMBER",2019,0,30),("OKT\211BER",2019,2,31),("NOVEMBER",2019,5,30),("DECEMBER",2019,0,31)]

> mlengths 2020
[31,29,31,30,31,30,31,31,30,31,30,31]

> leap 2020
True

> jan1 2019
2

> fstdays 2019
[2,5,5,1,3,6,1,4,0,2,5,0]

> calendar 2019
"JANU\193R 2019              FEBRU\193R 2019             MAREC 2019               \n                                                                           \nNed     6 13 20 27       Ned     3 10 17 24       Ned     3 10 17 24 31    \nPon     7 14 21 28       Pon     4 11 18 25       Pon     4 11 18 25       \nUto  1  8 15 22 29       Uto     5 12 19 26       Uto     5 12 19 26       \nStr  2  9 16 23 30       Str     6 13 20 27       Str     6 13 20 27       \n\352tv  3 10 17 24 31       \352tv     7 14 21 28       \352tv     7 14 21 28       \nPia  4 11 18 25          Pia  1  8 15 22          Pia  1  8 15 22 29       \nSob  5 12 19 26          Sob  2  9 16 23          Sob  2  9 16 23 30       \n                                                                           \nAPR\205L 2019               M\193J 2019                 J\218N 2019                 \n                                                                           \nNed     7 14 21 28       Ned     5 12 19 26       Ned     2  9 16 23 30    \nPon  1  8 15 22 29       Pon     6 13 20 27       Pon     3 10 17 24       \nUto  2  9 16 23 30       Uto     7 14 21 28       Uto     4 11 18 25       \nStr  3 10 17 24          Str  1  8 15 22 29       Str     5 12 19 26       \n\352tv  4 11 18 25          \352tv  2  9 16 23 30       \352tv     6 13 20 27       \nPia  5 12 19 26          Pia  3 10 17 24 31       Pia     7 14 21 28       \nSob  6 13 20 27          Sob  4 11 18 25          Sob  1  8 15 22 29       \n                                                                           \nJ\218L 2019                 AUGUST 2019              SEPTEMBER 2019           \n                                                                           \nNed     7 14 21 28       Ned     4 11 18 25       Ned  1  8 15 22 29       \nPon  1  8 15 22 29       Pon     5 12 19 26       Pon  2  9 16 23 30       \nUto  2  9 16 23 30       Uto     6 13 20 27       Uto  3 10 17 24          \nStr  3 10 17 24 31       Str     7 14 21 28       Str  4 11 18 25          \n\352tv  4 11 18 25          \352tv  1  8 15 22 29       \352tv  5 12 19 26          \nPia  5 12 19 26          Pia  2  9 16 23 30       Pia  6 13 20 27          \nSob  6 13 20 27          Sob  3 10 17 24 31       Sob  7 14 21 28          \n                                                                           \nOKT\211BER 2019             NOVEMBER 2019            DECEMBER 2019            \n                                                                           \nNed     6 13 20 27       Ned     3 10 17 24       Ned  1  8 15 22 29       \nPon     7 14 21 28       Pon     4 11 18 25       Pon  2  9 16 23 30       \nUto  1  8 15 22 29       Uto     5 12 19 26       Uto  3 10 17 24 31       \nStr  2  9 16 23 30       Str     6 13 20 27       Str  4 11 18 25          \n\352tv  3 10 17 24 31       \352tv     7 14 21 28       \352tv  5 12 19 26          \nPia  4 11 18 25          Pia  1  8 15 22 29       Pia  6 13 20 27          \nSob  5 12 19 26          Sob  2  9 16 23 30       Sob  7 14 21 28          \n                                                                           \n"

> picture ("Januar", 2020, 5, 28)
["Januar 2020              ","                         ","Ned     3 10 17 24       ","Pon     4 11 18 25       ","Uto     5 12 19 26       ","Str     6 13 20 27       ","\352tv     7 14 21 28       ","Pia  1  8 15 22          ","Sob  2  9 16 23          ","                         "]

> title "Januar" 2020
["Januar 2020              ","                         "]

> table 5 28
["Ned     3 10 17 24       ","Pon     4 11 18 25       ","Uto     5 12 19 26       ","Str     6 13 20 27       ","\352tv     7 14 21 28       ","Pia  1  8 15 22          ","Sob  2  9 16 23          ","                         "]

> entries 5 28
["     3 10 17 24   ","     4 11 18 25   ","     5 12 19 26   ","     6 13 20 27   ","     7 14 21 28   ","  1  8 15 22      ","  2  9 16 23      "]

> dates 5 28
[["   "],["   "],["   "],["   "],["   "],["  1"],["  2"],["  3"],["  4"],["  5"],["  6"],["  7"],["  8"],["  9"],[" 10"],[" 11"],[" 12"],[" 13"],[" 14"],[" 15"],[" 16"],[" 17"],[" 18"],[" 19"],[" 20"],[" 21"],[" 22"],[" 23"],[" 24"],[" 25"],[" 26"],[" 27"],[" 28"],["   "],["   "],["   "],["   "],["   "],["   "],["   "],["   "],["   "]]

> date 31 15
[" 15"]

> rjustify 4 "a"
"   a"

> height "abc"
3

> width ["ab", "cd"]
2

> above ["a","b"] ["c","d"]
["a", "b", "c", "d"]

> beside ["a", "b"] ["c", "d"]
["ac", "bd"]

> stack [["a", "b"], ["c","d"], ["e","f"]]
["a","b","c","d","e","f"]

> spread [["a", "b"], ["c","d"], ["e","f"]]
["ace","bdf"]

> empty (3,4)
["    ","    ","    "]

> block 2 [["aa", "bb"], ["cc","dd"], ["ee", "ff"], ["gg", "hh"]]
["aacc","bbdd","eegg","ffhh"]

> blockT 2 [["aa", "bb"], ["cc","dd"], ["ee", "ff"], ["gg", "hh"]]
["aaee","bbff","ccgg","ddhh"]

> group 3 [1, 2, 3, 4, 5, 6]
[[1,2,3],[4,5,6]]

> lframe (4,4) ["aa", "aa"]
["aa  ","aa  ","    ","    "]

> display ["aa  ","aa  ","    ","    "]
"aa  \naa  \n    \n    \n"

------------------------------------------------------------------- -}

-- ===================================================================
-- End of module
-- ===================================================================
