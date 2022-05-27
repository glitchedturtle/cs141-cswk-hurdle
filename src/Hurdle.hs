module Hurdle where

import Hurdle.Command
import Hurdle.Match

import Hurdle.Words (guessList, wordFrequencies, answerList)

import Data.Char (isAlpha, toUpper)
import Data.Maybe (mapMaybe)
import Data.List (delete, maximumBy)

import qualified Data.Map as Map
import qualified Data.IntMap.Strict as IntMap

--------------------------------------------------------------------------------
-- This file is your complete submission for the first coursework of CS141.
-- 
-- USER ID: 2147599
--
-- Before starting on this file, ensure that you have read the specification IN
-- ITS ENTIRETY and that you understand everything that is required from a good
-- solution.
--------------------------------------------------------------------------------
{-

    I have made small modifications to test/Spec.hs (reduced the number of attempts)
    and src/Hurdle/Words.hs (added a wordFrequencies, which reads from word-relative-frequency)

    I have also created a file RelFreq.hs, which is not directly used by Hurdle, but was used to
    process data

-}

-- | 1. Given an input guess, change it so that it is all upper case.        
--
-- [JUSTIFY]
-- Haskell base has a nice function that pretty much does exactly what we want - 'toUpper'
-- however, it differs slightly: If the character does not have an uppercase equivalent, it will simply return the
-- original char. We fix this by defining toUpper', which takes a char and returns the uppercase char if it's a valid character; and
-- nothing otherwise. Then, we use mapMaybe to call this function on every character in the string. (We import toUpper, isAlpha and mapMaybe from Data.Char)
-- mapMaybe is useful because it automatically discards the Nothings that are returned from the toUpper,
--  so we get a string that only contains alphabetical characters
toUpper' :: Char -> Maybe Char
toUpper' c
    | isAlpha c = Just (toUpper c)
    | otherwise = Nothing

normalise :: String -> String
normalise = mapMaybe toUpper'

--------------------------------------------------------------------------------
-- | 2. A valid guess is a one which appears in `guessList`.                 
--
-- [JUSTIFY]
-- The function is implemented by using the previously defined normalise, and then using the 'elem' function
-- to check whether the normalised word is contained in the guessList
-- We use `elem` instead of normal function application because it makes the code significantly cleaner 
-- - it removes the need for brackets.

isValid :: String -> Bool
isValid w = normalise w `elem` guessList

--------------------------------------------------------------------------------
-- | 3. Our program runs a little command line. Specific strings should be   
-- treated not as guesses, but as commands. See the specification for details.
--
-- [JUSTIFY]
-- parseCommand starts by normalising the string so that it matches for all cases. This had an unintended side effect
-- that give up works with any number of spaces, but is that much of a problem? We all need to make sacrifices for code readability


parseCommand :: String -> Command
parseCommand w = case normalise w of
    "LETTERS" -> ShowLetters
    "GIVEUP" -> GiveUp
    norm -> Guess norm

--------------------------------------------------------------------------------
-- | 4. Part one of the matching algorithm finds the exact matches.
-- For each position, give back IsExact if the two characters are the 
-- same, or IsNotExact if they are different. 
-- Implement this using explicit recursion. If you can see a more elegant 
-- solution, describe it in your justification.
--
-- [JUSTIFY]
-- WE use explicit recursion and pattern matching to iterate through both strings character by character
-- and compare them. the function pattern matches both "[] _" and "_ []", which defines 2 base cases: when either of the lists
-- are empty
-- Alternatively, this method could of been defined using implicit recursion. Specifically, by zipping both strings and then mapping
-- over the strings with a comparison (similar to what i have written).

exactMatches :: String -> String -> [ExactMatch]
exactMatches [] _ = []
exactMatches _ [] = []
exactMatches (a: as) (b : bs) =
    (if a == b then IsExact else IsNotExact) a
    : exactMatches as bs


--------------------------------------------------------------------------------
-- | 5. We want to keep track of the "unused" characters in the answer. First, 
-- we use up all of the exact matches. This function takes the exact matches and 
-- the answer and gives back all the characters not already exactly matched.
--
-- [JUSTIFY]
-- We define this function using explicit recursion: We iterate through each 'match' in the match list
-- if the match is an exact match, we remove one instance of that character from the string - otherwise we do nothing
removeExacts :: [ExactMatch] -> String -> String
removeExacts (c: cs) str = case c of
    IsExact c -> removeExacts cs $ delete c str
    IsNotExact c -> removeExacts cs str
removeExacts _ str = str -- if the list is empty, return whats left

--------------------------------------------------------------------------------
-- | 6. Follow the algorithm in the specification to correctly return the list 
-- of character matches, given the result of exactMatches and any unused 
-- characters of the answer.
-- 
-- [JUSTIFY]
-- we again use explicit recursion over the contents of the exact matches
-- if it's an 'IsExact'  match, we simply translate IsExact -> Exact and continue to recurse
-- however, if it is an IsNotExact match, it can either be Partial or None 
-- we use a guard to determine this, by checking whether the character is an element of the remaining characters

getMatches :: [ExactMatch] -> [Char] -> [Match]
getMatches [] _ = []
getMatches (e : es) rem = case e of
    IsExact c -> Exact : getMatches es rem
    IsNotExact c | c `elem` rem -> Partial : getMatches es (delete c rem)
    IsNotExact _ -> None : getMatches es rem


--------------------------------------------------------------------------------
-- | 7. Write the complete matching algorithm as a composition of the above 
-- three functions.
--
-- [JUSTIFY]
-- matchingAlgo is simply a composition of functions defined earlier in the project
matchingAlgo :: String -> String -> [Match]
matchingAlgo guess answer = getMatches exacts remaining
    where
        exacts = exactMatches guess answer
        remaining = removeExacts exacts answer

--------------------------------------------------------------------------------
-- | 8. Given a list of candidate words, remove those words which would not 
-- have generated the given match based on the guess that was made.
--
-- [JUSTIFY]
-- Here we use implicit recursion, as it seems more intuitive.
-- We use 'filter' to remove any element that can not be the answer (based on the provided information)
-- tests include non-normalised inputs, so we have to make sure that the inputs are normalised before comparison
eliminate :: String -> [Match] -> [String] -> [String]
eliminate guess ms = filter ((==) ms . matchingAlgo guess' . normalise)
    where
        guess' = normalise guess



--------------------------------------------------------------------------------
-- | 9. Based on the whole history of the game so far, return only those words 
-- from `guessList` which might still be the hidden word.
--
-- [JUSTIFY]
-- We use uncurry here to convert the eliminate method to a form which accepts the tuple.
-- then, we define eliminateAll' as the method which folds over the guess list using the eliminate' method
-- We define eliminateAll' because we want to use a method that uses a provided guess list in part 10, instead
-- of being forced to use Hurdle.Words.guessList. However the spec defines eliminateAll to use guessList,
-- so we define eliminateAll
eliminate' :: [String] -> (String, [Match]) -> [String]
eliminate' guessList gTuple = uncurry eliminate gTuple guessList

eliminateAll' :: [String] -> [(String, [Match])] -> [String]
eliminateAll' = foldl eliminate'

eliminateAll :: [(String, [Match])] -> [String]
eliminateAll = eliminateAll' guessList

--------------------------------------------------------------------------------
-- | 10. Using the above functions, write a function which produces a next guess 
-- based on the history of the game so far.
--
-- [JUSTIFY]
-- In my solution to part 10, I attempt to implement 3blue1brown's method for solving wordle (using information theory).
-- https://www.youtube.com/watch?v=v68zYyaEmEA
--
-- In simplest terms, for each word in the possible answers:
-- the bot calculates the probability of each outcome occurring and how much information each outcome
-- would provide if it happens. The sum of these values are known as the word's entropy.
--
-- The probability of each combination is determined by how many words, if they were the answer, would make
-- that specific combination of matches. For example, with a guess list of [ "DOG", "BOG", "COG", "BAT" ]
-- We can calculate the probability that DOG has [ Partial, Exact, Exact ] by matching "DOG" to every other element
-- A huge problem with this approach is that the probability distribution algorithm scales in polynomial time (n^2)
-- so for large guess lists the solution is very slow. 
--
-- I attempt to mitigate this in multiple ways:
-- First, I use the binary tree implementation of a map (Data.Map from containers (not in base)) in order to reduce the amount of time
-- it takes to sum each probability: We iterate over each word once and sum multiple counters, instead of iterating over
-- the entire word set for each combination. This greatly simplifies the code and i'm pretty sure it makes it much faster
-- which hopefully justifies the use of an external library
--
-- Secondly, for the case with the largest number of words to iterate over: nextGuess [],
-- we simply use a precomputed value for the first guess: "SALET" (https://www.youtube.com/watch?v=fRed0Xmc2Wg)
-- This greatly reduces the amount of time the algorithm takes
--
-- I wasn't entirely sure on the implementation of IntMap, but I attempted to optimise the solution further
-- by creating a hashing function for each combination, which would mean that during the sum, we would only
-- have to compare two ints, instead of two arrays of matches - although Data.Map might do this for us.
--
-- In my attempts to reduce the average guesses, I also implemented a relative frequency for each word. The premise was,
-- that for each value in guessList, it was more likely to be in the answerList if it was a commonly used english word
-- This was implemented with the data unigram_freq.csv and word-relative-frequency, with unigram_freq.csv from https://www.kaggle.com/rtatman/english-word-frequency
-- and word-relative-frequency being the processed version (RelFreq.hs) - however, this is irrelevent anyway
-- because I ended up using answerList

-- Mapping from [0 .. 5] to a prime number
p :: Int -> Int
p 0 = 3
p 1 = 5
p 2 = 7
p 3 = 11
p 4 = 13
p 5 = 17
p _ = 0

-- Convert a match array (of length up to 5) to an integer
hashMatch :: Int -> [Match] -> Int
hashMatch n (m: ms) = p n ^ (1 + fromEnum m) * hashMatch (n + 1) ms
hashMatch _ [] = 1

-- Calculate the probability distribution of each outcome of a guess
probDist :: String -> [String] -> IntMap.IntMap Double
probDist word wordList =
    IntMap.fromListWith (+) $ map (\(a,b)->(hashMatch 0 (matchingAlgo word a), b/total)) probs
    where
        probs = map (\word -> (word, calcWordProb word)) wordList
        total = sum $ map snd probs

-- Calculate the probability that a given word is selected
-- If it isnt found in the map, choose a default value of 1e-8 (what most uncommon words are)
calcWordProb :: String -> Double
calcWordProb w = Map.findWithDefault 1e-8 w wordFrequencies
--calcWordProb = const 1

-- Calculate the entropy of a potential guess, given a list of possible answers
entropy :: String -> [String] -> Double
entropy word wordList =
    sum $ map ((\p -> p * logBase 2 (1/p)).snd) $ IntMap.toList (probDist word wordList)

-- compare the second element of a tuple
cmpSnd :: Ord b => (a, b) -> (c, b) -> Ordering
cmpSnd (a_1, a_2) (b_1, b_2) = compare a_2 b_2

nextGuess :: [(String, [Match])] -> String
nextGuess [] = "SALET"
nextGuess prev =
    fst $ maximumBy cmpSnd $ map (\word -> (word, entropy word curWordList)) curWordList
    where
        curWordList = eliminateAll' answerList prev -- i use 'answerList' here, but we can also use guessList it just takes longer to compute
        -- it also makes the relative probability stuff kind of irrelevant but whatever