module Lib (module Lib)  where

caesar :: Int -> String -> String
caesar n = map (rotChar n)

rotChar :: Int -> Char -> Char
rotChar n c
  | isLower c = lowerRot n c
  | isUpper c = upperRot n c
  | otherwise = c

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot a n c = a !! ((indexOf c a + n) `mod` 26)

upperRot :: Int -> Char -> Char
upperRot = alphabetRot upperAlphabet

lowerRot :: Int -> Char -> Char
lowerRot = alphabetRot lowerAlphabet

bla :: Int -> String -> Char
bla _ [] = undefined
bla i (s:sx) =
  if i == 0
  then s
  else bla (i - 1) sx

listLength :: [a] -> Int
listLength ls =
  case ls of
    [] -> 0
    _:xs -> 1 + listLength xs

indexOf :: Char -> Alphabet -> Int
indexOf _ [] = undefined
indexOf c (s:sx) =
  if c == s
  then 0
  else 1 + indexOf c sx

isUpper :: Char -> Bool
isUpper c = c `elem` upperAlphabet

isLower :: Char -> Bool
isLower c = c `elem` lowerAlphabet

isDigit :: Char -> Bool
isDigit c = c `elem` digits

isMisc :: Char -> Bool
isMisc c = c `notElem` lowerAlphabet ++ upperAlphabet ++ digits

type Alphabet = [Char]

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']

digits :: Alphabet
digits = ['0' .. '9']
