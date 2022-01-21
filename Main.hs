module Main where 

import System.IO
import qualified Data.Map as Map
import Data.Map(Map, (!))
import Data.List
import Data.Maybe

data Creek = Creek (Int, Int) [((Int, Int), Int)] deriving Read
-- derivering w celu automatycznego utworzenia instacji i ulatwienia przy odczytaniu pliku

main::IO()
main = do
    putStrLn "Witaj, prosze o wskzanie nazwy pliku:"
    input <- getContents
    -- wczytanie nazwy pliku
    let inputLines = lines input
    let filename = head inputLines
    -- otworzenie pliku i przypisanie jego zawartości
    h <- openFile filename ReadMode
    content <- hGetContents h
    -- dump pliku
    putStrLn content

    -- wczytanie zawartości jako typu Creek    
    let creek :: Creek = read content
    let Creek (boardWidth, boardHeight) coords = creek
    -- wygenerowanie permutacji wypelnien
    let allFillings = generateFillings boardWidth boardHeight
    
    -- wypisanie poprawnej planszy w przypadku, gdy znajdziemy plansze spełniająca obywa warunki
    putStrLn $ head $ filter (\ content -> length content > 0) $ map (\ board -> if validateCreekCond1 creek board && validateCreekCond2 creek board then printBoard board (boardWidth, boardHeight) else "") $ allFillings
    
type Board = Map (Int, Int) Bool

-- generowanie możliwych wypełnień
-- wynikiem tej funkcji jest lista [((x,y), value,)]
generateFillings :: Int -> Int -> [Board]
generateFillings boardWidth boardHeight  =
    let size = boardWidth*boardHeight
        -- tworzymy liste booli - najpierw same False, a potem kolejno o jednego True więcej [True,False,...,False]
        lists = [take n (repeat True) ++ take (size-n) (repeat False) | n <- [0..size]]
        -- tworzymy unikalne permutacji każdego z elementó listy wygenerowanej wyżej
        listsPermutations = concatMap uniquePerms lists
        -- generujemy koordynaty na podstawie numer u w liście, x to jest 9\\4=2, a y to 9 mod 2=1 czyli (2,1)
        coords = map(\ n -> (div n boardHeight, mod n boardHeight)) [0..size-1]
        -- składamy koodynaty z permutacjami i uzyskujemy wyjściową listę
        maps = map(Map.fromList . zip coords) listsPermutations 
    in maps

-- sprawdzanie warunku na to czy wsrod sasiadow jest odpowiednia liczba zamalowanyc
validateCreekCond1 :: Creek -> Board -> Bool
validateCreekCond1 (Creek _ coords) board =
    -- generujemy liste sasiadow dla wskazanych koodynatow
    let neigbours x y = [(x,y), (x-1, y), (x-1, y-1), (x, y-1)]
        -- filtrujemy liste koordynatow tylko do tych ktorzy istnieja na planszy (member bierze element a potem mapę, a tu na odwrót dlatego flip)
        filterNeigbours = filter (flip Map.member board)
        -- zwracamy 1 i 0 zamiast True i False
        getSquareValue coords = if board ! coords then 1 else 0
        -- porównujemy wartość z koodynatów z sumą sąsiadów i zwracamy Bool'a
        result = all(\ ((x, y), value) -> value == (sum $ map getSquareValue $ filterNeigbours $ neigbours x y)) coords
    in result

-- sprawdzanie warunku czy białę tworzą jeden spójny obszar
validateCreekCond2 :: Creek -> Board -> Bool
validateCreekCond2 (Creek _ coords) board =
    -- przeliczamy ile jest białych na całęj planszy
    let whiteCount = Map.size $ Map.filter not board
        -- bierzemy pierwszy element white, o ile ostnieje dlatego acc jest Maybe, finalnie w acc mamy koodynaty pierwszego białego
        firstWhite = Map.foldrWithKey  (\ (x, y) value acc -> case acc of
                                                                Just value -> Just value
                                                                Nothing -> if not value then Just (x, y) else Nothing 
            ) Nothing board
        -- mający pierwszy biały to rekurencyjnym algorytmem dfs
        connectedWhites = dfs board $ fromJust firstWhite 
    -- jeśli mamy element bialy poczatkowy i liczba białych na planszy zgadza się z liczbą białych możliwych do osiagniecia to warunek jest spełniony
    in if isJust firstWhite then length connectedWhites == whiteCount else False

dfs :: Board -> (Int, Int) -> [(Int, Int)]
dfs board (x, y) =
    -- inicjalizujemy dfs'a z planszą, koodynatami poczatkowymi i pusta listą odiwedzonych białych
    dfsPom board (x, y) []

--
dfsPom :: Board -> (Int, Int) -> [(Int,Int)] -> [(Int, Int)]
dfsPom board (x, y) visited =
    -- pobieramy sąsiadów w każdą stronę danego poczatkowego białego
    let neigbours x y = [(x,y-1), (x+1, y), (x, y+1), (x-1, y)]
        -- filtrujemy zeby na pewno te punkty były w planszy
        filterNeigbours = filter (flip Map.member board)
    -- sprawdzamy czy sasiad jest bialy oraz czy nie był już odiwedzony
    -- jeśli nie to zapuszczamy tam rekurencyjnie dalej dfa
    -- finalnie uzyskujemy w acc listę białych która da się osiągnąć z punktu startowego
    in  foldr (\ (x, y) acc -> if board ! (x, y) || elem (x, y) acc then acc else dfsPom board (x, y) acc) ((x, y):visited) (filterNeigbours $ neigbours x y)

-- wyspiwanie planszy
printBoard :: Board -> (Int, Int) -> String
printBoard board (x, y) =
    let rows = [0..y-1]
        printedRows = map (\ rowNumber -> printRow board rowNumber x) rows
    in intercalate "\n" printedRows        
    
-- wypisywanie wiersza    
printRow :: Board -> Int -> Int -> String
printRow board rowNumber width =
    let xs = [0..width-1]
        coords = map (\ x -> (rowNumber, x)) xs
        values = map (\ coord -> board ! coord) coords
        chars = map (\ value -> if value then 'x' else 'o') values
    in intersperse ' ' chars

-- gotowa funkcja na stworzenie unikalnych permutacji, jeśli nie były unikalne to nawet dla 4x4 nie przeliczyłby tego sprawnie
-- http://www.camusestate-yp.co.kr/plugin/codemirror/mode/haskell/index.html

uniquePerms :: (Eq a) => [a] -> [[a]]
uniquePerms = permBag . makeBag
type Bag a = [(a, Int)]

makeBag :: (Eq a) => [a] -> Bag a
makeBag [] = []
makeBag (a:as) = mix a $ makeBag as
  where
    mix a []                        = [(a,1)]
    mix a (bn@(b,n):bs) | a == b    = (b,n+1):bs
                        | otherwise = bn : mix a bs

permBag :: Bag a -> [[a]]
permBag [] = [[]]
permBag bs = concatMap (\(f,cs) -> map (f:) $ permBag cs) . oneOfEach $ bs
  where
    oneOfEach [] = []
    oneOfEach (an@(a,n):bs) =
        let bs' = if n == 1 then bs else (a,n-1):bs
        in (a,bs') : mapSnd (an:) (oneOfEach bs)
    
    apSnd f (a,b) = (a, f b)
    mapSnd = map . apSnd
