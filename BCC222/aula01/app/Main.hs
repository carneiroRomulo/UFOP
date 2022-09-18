-- :t a (returns type of a)
-- :r   (refresh)
--  Classes básicas 
    -- Num (Int, Double, Float, Integer) 
    -- Eq (Bool, Char)
    -- Ord (Num, Bool, Char, List, Tuple)
    -- Show (Convert almost all types into a String)
-- List (Some functions)
-- null testa se uma lista é ou não vazia.
-- head retorna o primeiro elemento de uma lista.
-- tail retorna a cauda de uma lista.

import Data.List 
import Data.Char

--  AULA 01
sumUpTo :: Int -> Int
sumUpTo 1 = 1
sumUpTo n = n + sumUpTo(n - 1)

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n - 1)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- AULA 02
xor :: Bool -> Bool -> Bool
xor False True = True
xor True False = True
xor False False = False
xor True True = False

existsPositive :: [Int] -> Bool
existsPositive [] = False
existsPositive (x : xs) 
    | x > 0 = True
    | otherwise = existsPositive xs

--  AULA 03
bools :: [Bool] -> Bool
bools [] = True
bools (x : xs) = x && bools xs

-- nums :: [[Int]] -> Int
-- nums [] = 0
-- nums (x : xs) = nums xs ++ x

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a, a)
copy a = (a, a)

apply :: (a -> b) -> a -> b
apply p x = p x 

swap :: (a,b) -> (b,a)
swap (x, y) = (y, x)

-- AULA 04
minList :: [Int] -> Int
minList xs = minListAcc xs 999999
    where
        minListAcc [] acc = acc
        minListAcc (y : ys) acc
            | y < acc = minListAcc ys y
            | otherwise = minListAcc ys acc

andList :: [Bool] -> Bool
andList [] = True
andList (x : xs) = x && andList xs

orList :: [Bool] -> Bool
orList [] = False
orList (x : xs) = x || orList xs

indexOf :: Int -> [Int] -> Int --using accumulator
indexOf x ys = indexOfAcc x ys 0
    where
        indexOfAcc _ [] acc = -1
        indexOfAcc x (y : ys) acc
            | x == y = acc
            | otherwise = indexOfAcc x ys (acc+1) 

removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll x (y : ys)
    | x == y = removeAll x ys
    | otherwise = y : removeAll x ys


-- AULA 05
-- Capitalize all letters
-- capitalize :: String -> String
-- capitalize [] = []
-- capitalize (x : xs)
--     | isLetter x = toUpper x : capitalize xs
--     | otherwise = x : capitalize xs

-- Capitalize first letter of every word
capitalize :: String -> String
capitalize x =
    let capitalizeWord [] = []
        capitalizeWord (x : xs)
            | isLetter x = toUpper x : xs
            | otherwise = x : xs
    in unwords (map capitalizeWord (words x))

primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], isPrime x]
    where
        isPrime x = (length (factors x)) == 2
        factors x = [ y | y <- [1 .. x], x `mod` y == 0 ]

withoutPrimes :: [Int] -> [Int]
withoutPrimes n = [x | x <- n, isNotPrime x]
    where
        isNotPrime x = (length (factors x)) /= 2
        factors x = [ y | y <- [1..x], x `mod` y == 0] 

-- AULA 06
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- takeWhile _ [] = []
-- takeWhile p (x : xs)
--     | p x = x : takeWhile p xs
--     | otherwise = []

-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- takeWhile f = foldr step []
--     where
--         step x ac = f x : ac

-- PROVA 01 - TESTE 01
votes :: [String]
votes = ["A", "B", "A", "C", "A", "D", "B", "C", "A"]

count :: String -> [String] -> Int
count x xs = length (filter (== x) xs)

candidates :: [String] -> [String]
candidates [] = []
candidates (x : xs) = x : candidates (filter (/= x) xs)

result :: [String] -> [(String, Int)]
result xs = [(c, count c xs) | c <- candidates xs]

winner :: [String] -> String
winner xs = head (map fst (result xs))

divide :: Int -> [a] -> ([a], [a])
divide _ [] = ([], []) 
divide 0 xs = ([], xs)
divide n xs = (take n xs, drop n xs)

-- PROVA 01 - TESTE 02
-- Questão 01 - Retorne uma lista de inteiros em que todo numero impar presente na lista é elevado ao quadrado
questao01 :: [Integer] -> [Integer]
questao01 [] = []
questao01 (x : xs)
    | odd x = x^2 : questao01 xs
    | otherwise = x : questao01 xs

-- Questão 02 - Rotacionar os valores de uma tripla 'times' vezes
data Times = Zero | One | Two
questao02 :: Times -> (a, a, a) -> (a, a, a)
questao02 Zero t = t
questao02 One (x, y, z) = (z, x, y)
questao02 Two (x, y, z) = (y, z, x)

-- Questão 03
type Name = String
type Phone = String
type Email = String
data Client = Client Name Phone Email deriving (Eq, Show)

-- Dizemos que a informação de um cliente é válida se:
-- a) O nome do cliente possui pelo menos 3 caracteres e é formado exclusivamente por letras e espaços.
-- b) A informação de telefone é composta apenas por dígitos
-- c) A string de email deve conter o caractere @ e ter tamanho maior que 3.
-- Com base nessas informações, desenvolva a função:

validateName :: Name -> Bool
validateName name = length name > 2 && name == [letter | letter <- name, isLetter letter || isSpace letter]

validatePhone :: Phone -> Bool
validatePhone phone = phone == [digit | digit <- phone, isDigit digit]

validateEmail :: Email -> Bool
validateEmail email = length email > 2 && '@' `elem` email

question03 :: Client -> Bool
question03 (Client name phone email) = validateName name && validatePhone phone && validateEmail email

--  Questão 04
data Error = NameLengthError -- invalid size
    | NameCharactersError -- name with non-letters and space characters
    | PhoneError -- phone with non numeric chars.
    | EmailSizeError -- invalid size
    | EmailCharError -- lacking `@`
    deriving (Eq, Show)

data Validation = Ok | Failure [Error] deriving (Eq, Show)

validateNameQ4 :: Name -> Maybe Error
validateNameQ4 name
    | length name <= 2 = Just NameLengthError
    | name /= filter (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ [' ']) name = Just NameCharactersError
    | otherwise = Nothing

validatePhoneQ4 :: Phone -> Maybe Error
validatePhoneQ4 phone
    | phone /= filter (`elem` ['0'..'9']) phone = Just PhoneError
    | otherwise = Nothing

validateEmailQ4 :: Email -> Maybe Error
validateEmailQ4 email
    | length email <= 2 = Just EmailSizeError
    | not ('@' `elem` email) = Just EmailCharError
    | otherwise = Nothing

unpackErrors :: [Maybe Error] -> [Error]
unpackErrors [Nothing] = []
unpackErrors [Just a] = [a]
unpackErrors (Nothing : xs) = unpackErrors xs
unpackErrors (Just x : xs) = x : unpackErrors xs

question04 :: Client -> Validation
question04 (Client name phone email)
    | [validateNameQ4 name, validatePhoneQ4 phone, validateEmailQ4 email] == [Nothing, Nothing, Nothing] = Ok
    | otherwise = Failure (unpackErrors [validateNameQ4 name, validatePhoneQ4 phone, validateEmailQ4 email])


allNats :: [Int] -> [Int]
allNats xs = length [x | x <- xs, x > 0] > 0

pairs :: [Int] -> [(Int, Int)]
-- pairs [] = []
-- pairs (x:xs)
--     | length xs > 1 = (x,head xs) : pairs (tail xs)
--     | otherwise = (x,xs) : []
pairs (x:xs)
    | length xs > 1 = (x,head xs) : pairs (tail xs)
    | otherwise = []

-- uncompress :: [Int] -> [Int]
uncompress xs = foldr step base input
uncompreess [] = []
    where
        step = 
        base = 
        input = 