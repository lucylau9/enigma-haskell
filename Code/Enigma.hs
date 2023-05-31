{-- Stub for the grading assignment. Fill it in, making sure you use good
 -- functional style, and add comments (including replacing those that are
 -- already here).
--}

module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type
  import Data.List
  import Data.Ord

  import Debug.Trace

{- Part 1: Simulation of the Enigma -}

  type Rotor = (String, Int) -- (wirings, pin)
  type Reflector = [(Char, Char)]
  type Offsets = (Int, Int, Int) -- offsets for LR, MR, RR
  type Stecker = [(Char, Char)]
  
  -- Assumes rotor order is LR MR RR
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker

  -- Encodes an entire message using an Enigma
  encodeMessage :: String -> Enigma -> String
  encodeMessage [] _ = []
  -- Simple enigma
  encodeMessage message (SimpleEnigma lr mr rr reflect ofst) = 
    encodeChar c (SimpleEnigma lr mr rr reflect ofst) : encodeMessage rest newEnigma
    where (c:rest) = sanitise message
          newEnigma = SimpleEnigma lr mr rr reflect newOfst
          newOfst = advanceRotors lr mr rr ofst
  -- Steckered enigma
  encodeMessage message (SteckeredEnigma lr mr rr reflect ofst plug) =
    encodeChar c (SteckeredEnigma lr mr rr reflect ofst plug) : encodeMessage rest newEnigma
    where (c:rest) = sanitise message
          newEnigma = SteckeredEnigma lr mr rr reflect newOfst plug
          newOfst = advanceRotors lr mr rr ofst
    
  -- Encodes a char using an Enigma
  encodeChar :: Char -> Enigma -> Char
  -- Simple enigma
  encodeChar c (SimpleEnigma lr mr rr reflect ofst)
    = reversePass
      where newOfst = advanceRotors lr mr rr ofst
            or = thd' newOfst
            om = snd' newOfst
            ol = fst' newOfst
            rotorPass = rotorChar (rotorChar (rotorChar c rr or) mr om) lr ol
            newC = pairChar rotorPass reflect
            reversePass = reverseRotor (reverseRotor (reverseRotor newC lr ol) mr om) rr or
  -- Steckered enigma
  encodeChar rawC (SteckeredEnigma lr mr rr reflect ofst plug)
    = pairChar reversePass plug
      where c = pairChar rawC plug
            newOfst = advanceRotors lr mr rr ofst
            or = thd' newOfst
            om = snd' newOfst
            ol = fst' newOfst
            rotorPass = rotorChar (rotorChar (rotorChar c rr or) mr om) lr ol
            newC = pairChar rotorPass reflect
            reversePass = reverseRotor (reverseRotor (reverseRotor newC lr ol) mr om) rr or

  {- Takes a character and a rotor and passes the character through the rotor,
  taking into account the offsets -}
  rotorChar :: Char -> Rotor -> Int -> Char
  rotorChar c (wirings, _) ofst = output
    where index = ( alphaPos c + ofst ) `mod` 26
          wiredC = wirings !! index
          output = intToChar ((alphaPos wiredC - ofst) `mod` 26)

  -- Pass character through a rotor in reverse
  reverseRotor :: Char -> Rotor -> Int -> Char
  reverseRotor c (wirings, _) ofst = intToChar index
    where newC = intToChar ((alphaPos c + ofst) `mod` 26)
          index = (fromJust (elemIndex newC wirings) - ofst) `mod` 26

  -- Advances all rotors at once
  advanceRotors :: Rotor -> Rotor -> Rotor -> Offsets -> Offsets
  advanceRotors _ (_, pinM) (_, pinR) (ol, om, or) 
    | pinM == om && pinR == or = ((ol+1) `mod` 26, (om+1) `mod` 26, (or+1) `mod` 26)
    | pinR == or = (ol, (om+1) `mod` 26, (or+1) `mod` 26)
    | otherwise = (ol, om, (or+1) `mod` 26)

  -- Maps a character to another using a reflector or stecker
  pairChar :: Char -> [(Char, Char)] -> Char
  pairChar c steckers | x == c = y
                      | otherwise = x
                      where (x, y) = findPair c steckers

  -- Filters a tuple in a list of tuples that contains the item
  findPair :: Eq a => a -> [(a, a)] -> (a, a)
  findPair item [] = (item, item) 
  findPair item ((x, y):rest) | x == item = (x, y)
                               | y == item = (x, y)
                               | otherwise = findPair item rest

  -- Accessing values in thruple
  fst' :: (a, b, c) -> a
  fst' (x, _, _) = x

  snd' :: (a, b, c) -> b
  snd' (_, y, _) = y

  thd' :: (a, b, c) -> c
  thd' (_, _, z) = z

  -- Remove spaces and capitalises every character
  sanitise :: String -> String
  sanitise s = [toUpper c | c <- s, c `notElem` delimiter]

  delimiter = " .,!?/():;@[]£$%^&*{}#~-_+=<>'|`\\\""

{- Part 2: Finding the Longest Menu -}

  type Menu = [Int]
  type Crib = [(Char, Char)]

  longestMenu :: Crib -> Menu
  longestMenu crib = longest allMenus
    where cribIndex = [0..(length crib - 1)]
          allMenus = [findLongestMenuAt [[x]] crib | x <- cribIndex]
          longest = maximumBy (comparing length)

  -- Finds a longest menu at a position
  findLongestMenuAt :: [[Int]] -> Crib -> Menu
  findLongestMenuAt index crib | stepIndex == index = head index
                               | otherwise = findLongestMenuAt stepIndex crib
                                where stepIndex = chain index crib

  -- Adds a new index onto current chain
  chain :: [[Int]] -> Crib -> [[Int]]
  chain ls crib | null valid = ls
                | null (newChain ls crib) = ls
                | otherwise = newChain ls crib
    where valid = [l | l <- ls, hasMatch (snd (crib !! last l)) crib]
          newChain [] _ = []
          newChain (x:xs) crib = [x ++ [next] | next <- nextIndex x crib, next `notElem` x]
                                ++ newChain xs crib

  -- Finds list of next indexes given a current chain
  nextIndex :: [Int] -> Crib -> [Int]
  nextIndex current crib = [i | i <- matching, i `notElem` current]
    where matching = findMatching c crib 0
          c = snd (crib !! last current)

  -- Checks if a char has a match in crib
  hasMatch :: Char -> Crib -> Bool
  hasMatch _ [] = False
  hasMatch c (pair:rest) | fst pair == c = True
                          | otherwise = hasMatch c rest

  -- Returns a list of indexes where the element is the first of a pair
  findMatching :: Char -> Crib -> Int -> [Int]
  findMatching _ [] _ = []
  findMatching item (x:xs) n | item == fst x = n : findMatching item xs (n+1)
                           | otherwise = findMatching item xs (n+1)

{- Part 3: Simulating the Bombe -}
  
  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  breakEnigma crib | isNothing result = Nothing
                   | otherwise = Just (fst (fromJust result), removeSame (snd (fromJust result)))
    where menu = longestMenu crib
          result = testOffsets (Just (0, 0, 0)) menu crib $ SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0, 0, 0)

  -- Tests all possible offsets
  testOffsets :: Maybe Offsets -> Menu -> Crib -> Enigma -> Maybe (Offsets, Stecker)
  testOffsets ofst menu crib (SimpleEnigma lr mr rr refl _) 
    | isNothing ofst = Nothing
    | isNothing config =  testOffsets newOfst menu crib $ SimpleEnigma lr mr rr refl (fromJust newOfst)
    | otherwise = Just (fromJust ofst, fromJust config)
    where newOfst = incrementOffsets (fromJust ofst)
          config = testInitial 0 menu crib (SimpleEnigma lr mr rr refl (fromJust ofst))

  -- Tests all possible start pairings
  testInitial :: Int -> Menu -> Crib -> Enigma -> Maybe Stecker
  testInitial count menu crib enigma | count > 25 = Nothing
                                     | steckerValid config = Just config
                                     | otherwise = testInitial (count+1) menu crib enigma
    where config = filterUnique $ followMenu menu [initialPair] crib enigma
          initialPair = (fst (crib !! head menu), intToChar count)

  -- Checks if a configuration is possible
  steckerValid :: Stecker -> Bool
  steckerValid plug = all (`pairValid` plug) plug

  -- Checks if a pair is valid in the plugboard
  pairValid :: (Char, Char) -> Stecker -> Bool
  pairValid pair plug = (fst pair `notElem` newPlug) && (snd pair `notElem` newPlug)
    where newPlug = concat2 (unzip (delete pair plug))

  -- Follows a menu and outputs a potential configuration for plugboard
  followMenu :: Menu -> Stecker -> Crib -> Enigma -> Stecker
  followMenu [] plug _ _ = [last plug]
  followMenu (index:rest) plug crib enigma = newPair : followMenu rest newPlug crib enigma
      where c = fst (crib !! index)
            newPair = createPair index plug crib enigma
            (x, y) = newPair
            newPlug = newPair : plug

  -- Removes duplicate pairs
  filterUnique :: [(Char, Char)] -> [(Char, Char)]
  filterUnique [] = []
  filterUnique ((x,y):rest) | (x,y) `elem` rest || (y,x) `elem` rest = filterUnique rest
                            | otherwise = (x,y) : filterUnique rest

  -- Removes pairs where a character is paired to itself
  removeSame :: [(Char, Char)] -> [(Char, Char)]
  removeSame [] = []
  removeSame ((x,y):rest) | x==y = removeSame rest
                          | otherwise = (x,y) : removeSame rest
  
  -- Runs a char through an enigma and outputs a possible pairing
  createPair :: Int -> Stecker -> Crib -> Enigma -> (Char, Char)
  createPair index plug crib (SimpleEnigma lr mr rr refl ofst) = newPair
    where newOfst = nUpdateOffsets index lr mr rr ofst
          plain = fst (crib !! index)
          input = pairChar plain plug
          output = encodeChar input $ SimpleEnigma lr mr rr refl newOfst
          newPair = createPair' output index crib

  createPair' :: Char -> Int -> Crib -> (Char, Char)
  createPair' c index crib = (c, cipherChar)
    where cipherChar = snd (crib !! index)

  -- Increments offsets, but outputs nothing if (25, 25, 25) is input
  incrementOffsets :: Offsets -> Maybe Offsets
  incrementOffsets (ol, om, or) | or == 25 && om == 25 && ol == 25 = Nothing
                                | or == 25 && om == 25 = Just (ol+1, 0, 0)
                                | or == 25 = Just (ol, om+1, 0)
                                | otherwise = Just (ol, om, or+1)

  -- Updates offsets based on current index
  nUpdateOffsets :: Int -> Rotor -> Rotor -> Rotor -> Offsets -> Offsets
  nUpdateOffsets index lr mr rr ofst = iterate (advanceRotors lr mr rr) ofst !! index

  -- Concat but for a pair
  concat2 :: ([a], [a]) -> [a]
  concat2 (xs, ys) = xs ++ ys

{- Useful definitions and functions -}

   -- substitution cyphers for the Enigma rotors
   -- as pairs of (wirings, knock-on position)
   -- knock-on position is where it will cause the next left wheel to
   -- advance when it moves past this position
 
        --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  rotor1=("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  rotor2=("AJDKSIRUXBLHWTMCQGZNPYFVOE",5::Int)
  rotor3=("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int)
  rotor4=("ESOVPZJAYQUIRHXLNFTGKDCMWB",10::Int)
  rotor5=("VZBRGITYUPSDNHLXAWMJQOFECK",0::Int)

  {- the standard Enigma reflector (Reflector B)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W
  -}
  reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]

  {- alphaPos: given an uppercase letter, returns its index in the alphabet
     ('A' = position 0; 'Z' = position 25)
   -}
  alphaPos :: Char -> Int
  alphaPos c = ord c - ord 'A'

  -- Takes an int and turns it into a char
  intToChar :: Int -> Char
  intToChar n = toUpper (chr (ord 'a' + n))

