> import Data.List
> import Data.Char

Zadanie 1.
Funkcja, która zamienia liczbę dodatnią na ciąg cyfr jej rozwinięcia dziesiętnego. Przyda się przy tym funkcja unfoldr z modułu
Data.List i standardowa funkcja reverse.  Pokaż, jak  skorzystać  z  modułu Data.List w ghci i  jak  sprawdzić  typ  tych funkcji.

> explode :: Integer -> [Integer]
> explode = reverse . unfoldr
>   ( \ n -> if n > 0
>     then Just(n `mod` 10, n `div` 10)
>     else Nothing)

Zadanie 2.
Funkcja odwrotna do powyższej. Spróbuj zaprogramować ją z użyciem standardowej funkcji foldl (przyda się przy tym standardowa funkcja fst).

> implode :: [Integer] -> Integer
> implode = fst . foldl (\(a, b) c -> (b + c, (b + c) * 10)) (0, 0)

Zadanie 3.
Funkcja  kodująca  i  dekodująca  napisy  zaszyfrowane  szyfrem Cezara z kluczem 13. Tu przyda się pewnie standardowa funkcja map i różne funkcje z modułu Data.Char. Pokaż gdzie szukać dokumentacji tego modułu.

> rot13 :: String -> String
> rot13 = map transform
>   where transform c = chr $ (ord c - offset c + 13) `mod` 26 + offset c
>         offset c | isUpper c = 65 | isLower c = 97

Zadanie 4.
Funkcja tworząca listę podciągów podanej listy. Podciągiem nazywamy tu  dowolną  listę  powstałą  przez  pominięcie  wybranych  elementów  oryginalnej  listy. Lista długości n ma 2^n podciągów.

> subsequences' :: [a] -> [[a]]
> subsequences' [] = [[]]
> subsequences' (x:xs) = map (x:) subs ++ subs
>   where subs = subsequences' xs

Zadanie 5.
Funkcja tworząca listę prefiksów podanej listy. Lista długości n ma n+1 prefiksów.

> inits' :: [a] -> [[a]]
> inits' = map reverse . (scanl (flip (:)) [])
>
> inits'' :: [a] -> [[a]]
> inits'' = foldl (\b a -> (head b ++ [a]) : b) [[]]

Zadanie 6.
Funkcja tworząca listę sufiksów podanej listy. Lista długości n ma n+1 sufiksów.

> tails' :: [a] -> [[a]]
> tails' [] = [[]]
> tails' l @ (_:xs) = l : tails xs
>
> tails'' :: [a] -> [[a]]
> tails'' = ([]:) . unfoldr (\l -> if null l then Nothing else Just(l, tail l))

Zadanie 7.
Funkcja tworząca listę segmentów podanej listy. Segmentem nazywamy tu dowolną listę powstałą przez odrzucenie dowolnej liczby początkowych i końcowych elementów listy. Lista długości n ma n(n+1)/2 + 1 segmentów.

> segments :: [a] -> [[a]]
> segments l = [] : [x | xs <- tails' l, x <- inits' xs, not $ null x]

Zadanie 8.
Funkcja tworząca listę permutacji podanej listy. Skorzystaj z serwisu hoogle.haskell.org i pokaż jak sprawdzić, jakie jeszcze inne funkcje dostępne w bibliotekach standardowych Haskella mają typ [a] -> [[a]].

> iperm :: [a] -> [[a]]
> iperm [] = [[]]
> iperm (x:xs) = [zs | ys <- iperm xs, zs <- insert x ys]
>   where
>     insert :: a -> [a] -> [[a]]
>     insert x [] = [[x]]
>     insert x l@(y:ys) = (x : l) : (map (y:) (insert x ys))
>
> sperm :: Eq a => [a] -> [[a]]
> sperm [] = [[]]
> sperm xs = [y : ys | y <- xs, ys <- sperm (delete y xs)]
>   where
>     delete :: Eq a => a -> [a] -> [a]
>     delete x [] = []
>     delete x (y:ys) = if x == y then ys else y : delete x ys

Zadanie 9.
Funkcja scalająca uporządkowane listy (z powtórzeniami).

> merge :: Ord a => [a] -> [a] -> [a]
> merge [] xs = xs
> merge xs [] = xs
> merge (x:xs) (y:ys) = if x < y
>   then x : merge xs (y:ys)
>   else y : merge (x:xs) ys

Zadanie 10.
Funkcja sortująca podaną liczbę elementów podanej listy zgodnie z algorytmem Mergesort. Przyda się tu funkcja merge z poprzedniego punktu i standardowa funkcja drop.

> msortPrefix :: Ord a => Int -> [a] -> [a]
> msortPrefix 0 xs = []
> msortPrefix n xs = sort n (take n xs) ++ drop n xs
>   where
>     sort 1 xs = xs
>     sort n xs = let halfN = n `div` 2
>                     (ys, zs) = split halfN xs
>       in merge (sort halfN ys) (sort (n - halfN) zs)
>     split k ys = (take k ys, drop k ys)

Zadanie 11.
Funkcja sortująca podaną listę. Przyda się tu funkcja z poprzedniego punktu i standardowa funkcja length.

> msort :: Ord a => [a] -> [a]
> msort xs = msortPrefix (length xs) xs

Zadanie 12.
Funkcja sortująca podaną listę zgodnie z algorytmem Quicksort

> qsort :: Ord a => [a] -> [a]
> qsort [] = []
> qsort (x:xs) = qsort [y | y <- xs, y <= x] ++ x : qsort [y | y <- xs, y > x]

Zadanie 13.
Funkcja sortująca podaną listę zgodnie z algorytmem Insertion Sort.

> isort :: Ord a => [a] -> [a]
> isort xs = isortAux xs []
>   where
>     isortAux []      e = e
>     isortAux (x:xs) ys = isortAux xs (insert x ys)
>     insert y []     = [y]
>     insert y (z:zs) = if y <= z then y : z : zs else z : insert y zs

Zadanie 14.
Funkcja sortująca podaną listę zgodnie z algorytmem Selection Sort.

> ssort :: Ord a => [a] -> [a]
> ssort [] = []
> ssort (x:xs) = smallest : ssort rest
>   where
>     (smallest, rest) = min xs x []
>     min []     e acc = (e, acc)
>     min (y:ys) e acc = if y < e then min ys y (e:acc) else min ys e (y:acc)

Zadanie 15.
Funkcja sprawdzająca, czy podany element występuje na liście.

> elem' :: Eq a => a -> [a] -> Bool
> elem' e []     = False
> elem' e (x:xs) = if e == x then True else elem' e xs

Zadanie 16.
Funkcja wstawiająca podany element pomiędzy każdą parę sąsiednich elementów podanej listy.
Np. wartością intersperse ’,’ "abcde" jest "a,b,c,d,e".

> intersperse' :: a -> [a] -> [a]
> intersperse' e []     = []
> intersperse' e (x:xs) = x : e : intersperse' e xs
