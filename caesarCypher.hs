-- This is a Caesar Cypher written in haskell. Inspired by a chapters 1-6 in Cracking Codes with Python by Al Sweigart

-- Module to iterate a list and print each element on a single line.
import Control.Monad (forM_)

-- Defines the characters that are valid for this cypher
symbols :: String
symbols = ['A'..'Z']++['a'..'z']++['0'..'9']++" !@#$%^&*()_-+=/.,;:'"

-- Defines the valid keys
keys :: [Int]
keys = [0..25]

keyError :: String
keyError = " ** Error: Invalid Key ** "

charError :: String
charError = " ** Error: invalid Char ** "

-- Defines how many valid characters there are 
sL :: Int
sL = length symbols

-- takes the key and the integer that corrisponds with the symbol and encrypts the symbol
encrypt :: Int -> Int -> Char
encrypt key i

-- this allows the cypher to wrap around the alphabet if the key converts the integer to someting that is outside the rage of valid
-- characters
    | (i + key) < 0   = last $ take (sL - abs ((i+1) + key)) symbols
    | (i + key) < sL = last $ take ((i+1) + key) symbols 
    | otherwise       = last $ take ((i-(sL-1)) + key) symbols

-- assigns a number to each character in the list of valid symbols
charIndex :: (Num a, Enum a) => Char -> a
charIndex ch = head [y | (x,y) <- zip symbols [0..], x == ch]

encryptChar :: Int -> Char -> Char
encryptChar key ch = encrypt key $ charIndex ch

encryptMessage :: Int -> String -> String
encryptMessage _ [] = []
encryptMessage key (x:xs)
    | key  `notElem` keys = keyError
    | x `notElem` symbols = charError
    | otherwise           = encryptChar key x : encryptMessage key xs

decryptMessage :: Int -> String -> String
decryptMessage _ [] = []
decryptMessage key (x:xs)
    | key  `notElem` keys = keyError
    | x `notElem` symbols = charError
    | otherwise           = encryptChar (negate key) x : decryptMessage key xs

-- Takes in a string and iterates through the keys, exporting a decryption for each key on a new line.
bruteForce :: String -> IO ()
bruteForce message = forM_ messageList print
    where 
        messageList = ["Key #" ++ show x ++ ": " ++ decryptMessage x message | x <- keys]

