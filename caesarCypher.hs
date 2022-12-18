-- This is a Caesar Cypher written in haskell.

symbols :: String
symbols = ['A'..'Z']++['a'..'z']++['0'..'9']++" !@#$%^&*()_-+=/.,;:"

keys :: [Int]
keys = [0..25]

sL :: Int
sL = length symbols

encrypt :: Int -> Int -> Char
encrypt key i
    | (i + key) < 0   = last $ take (sL - abs ((i+1) + key)) symbols
    | (i + key) < sL = last $ take ((i+1) + key) symbols 
    | otherwise       = last $ take ((i-(sL-1)) + key) symbols

charIndex :: (Num a, Enum a) => Char -> a
charIndex ch = head [y | (x,y) <- zip symbols [0..], x == ch]

encryptChar :: Int -> Char -> Char
encryptChar key ch = encrypt key $ charIndex ch

encryptString :: Int -> String -> String
encryptString _ [] = []
encryptString key (x:xs)
    | key  `notElem` keys = "Error: Invalid Key"
    | x `notElem` symbols = " ** Error: invalid Char ** "
    | otherwise           = encryptChar key x : encryptString key xs

decryptString :: Int -> String -> String
decryptString _ [] = []
decryptString key (x:xs)
    | key  `notElem` keys = "Error: Invalid Key"
    | x `notElem` symbols = " ** Error: invalid Char ** "
    | otherwise           = encryptChar (negate key) x : decryptString key xs

-- Takes in a string and iterates through the keys, exporting a decrytion for each key on a new line.
bruteForce :: [String -> String]
bruteForce = map decryptString keys

