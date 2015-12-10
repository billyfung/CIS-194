--Validating Credit Card Numbers

--First we find the digits of the number

toDigits:: Integer -> [Integer]
toDigits x 
    | x <= 0    = []
    | otherwise =  toDigits (x `div` 10) ++ [x `mod` 10]

-- recursively grab n-1 digits till empty from x 
-- perform `div` 10 and then add to list of the last digit

toDigitsRev:: Integer -> [Integer]
toDigitsRev x
    | x <=0     =[]
    | otherwise = (x `mod`10):toDigitsRev(x `div` 10)

--if empty or negative input, return empty list
-- x mod 10 grabs the last digit and puts it as the head
-- tail is then input recursively put into `div` 10 till empty

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs)
    | length(x:y:zs) `mod` 2 == 0 = x*2:y:doubleEveryOther zs
    | otherwise                   = x:y*2:doubleEveryOther zs

--1st pattern, if empty return empty list
--2nd pattern, if only 1 element, return 1 
--3rd pattern, break list into 3 parts and then check length of input
--for even length, we double 1st, 3rd and then tail is put back into function
--for odd, we leave the first, and double the 2nd, then tail is put back into function

sumDigits :: [Integer]->Integer
sumDigits [] = 0
sumDigits (x:[]) = x
sumDigits x = sum $ concat (map toDigits x)

