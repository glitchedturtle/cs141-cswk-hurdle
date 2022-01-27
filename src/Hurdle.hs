module Hurdle where

import Hurdle.Command
import Hurdle.Match
  

--------------------------------------------------------------------------------
-- This file is your complete submission for the first coursework of CS141.
-- 
-- USER ID: 
--
-- Before starting on this file, ensure that you have read the specification IN
-- ITS ENTIRETY and that you understand everything that is required from a good
-- solution.
--------------------------------------------------------------------------------


-- | 1. Given an input guess, change it so that it is all upper case.        
--
-- [JUSTIFY]
normalise :: String -> String
normalise = error "Not implemented"


--------------------------------------------------------------------------------
-- | 2. A valid guess is a one which appears in `guessList`.                 
--
-- [JUSTIFY]
isValid :: String -> Bool
isValid = error "Not implemented"


--------------------------------------------------------------------------------
-- | 3. Our program runs a little command line. Specific strings should be   
-- treated not as guesses, but as commands. See the specification for details.
--
-- [JUSTIFY]
parseCommand :: String -> Command
parseCommand = error "Not implemented"


--------------------------------------------------------------------------------
-- | 4. Part one of the matching algorithm finds the exact matches.
-- For each position, give back IsExact if the two characters are the 
-- same, or IsNotExact if they are different. 
-- Implement this using explicit recursion. If you can see a more elegant 
-- solution, describe it in your justification.
--
-- [JUSTIFY]
exactMatches :: String -> String -> [ExactMatch]
exactMatches = error "Not implemented"


--------------------------------------------------------------------------------
-- | 5. We want to keep track of the "unused" characters in the answer. First, 
-- we use up all of the exact matches. This function takes the exact matches and the answer and gives back all the characters not already exactly matched.
--
-- [JUSTIFY]
removeExacts :: [ExactMatch] -> String -> String
removeExacts = error "Not implemented"


--------------------------------------------------------------------------------
-- | 6. Follow the algorithm in the specification to correctly return the list 
-- of character matches, given the result of exactMatches and any unused 
-- characters of the answer.
-- 
-- [JUSTIFY]
getMatches :: [ExactMatch] -> [Char] -> [Match]
getMatches = error "Not implemented"


--------------------------------------------------------------------------------
-- | 7. Write the complete matching algorithm as a composition of the above three functions.
--
-- [JUSTIFY]
matchingAlgo :: String -> String -> [Match]
matchingAlgo = error "Not implemented"


--------------------------------------------------------------------------------
-- | 8. Given a list of candidate words, remove those words which would not 
-- have generated the given match based on the guess that was made.
--
-- [JUSTIFY]
eliminate :: String -> [Match] -> [String] -> [String]
eliminate = error "Not implemented"



--------------------------------------------------------------------------------
-- | 9. Based on the whole history of the game so far, return only those words from `guessList` which might still be the hidden word.
--
-- [JUSTIFY]
eliminateAll :: [(String, [Match])] -> [String]
eliminateAll = error "Not implemented"


--------------------------------------------------------------------------------
-- | 10. Using the above functions, write a function which produces a next guess based on the history of the game so far.
--
-- [JUSTIFY]
nextGuess :: [(String, [Match])] -> String
nextGuess = error "Not implemented"