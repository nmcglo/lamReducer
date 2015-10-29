import PA1Helper
import Data.Set as Set
import Data.List as List

-- Haskell representation of lambda expression
-- In Lambda Lexp Lexp, the first Lexp should always be Atom String
-- data Lexp = Atom String | Lambda Lexp Lexp | Apply Lexp  Lexp 

-- Given a filename and function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
-- runProgram :: String -> (Lexp -> Lexp) -> IO()

----------GLOBALS-----------------------------------

--Quick and dirty creation of set of alphabet character strings
alphaChar = ['a' .. 'z']
alphaStr = List.map strFromChar alphaChar
alphaSet = fromList alphaStr


----------TOOLS-----------------------------------------

--Simple converter from char into a string
strFromChar :: Char -> String
strFromChar c = [c]

--removes all instances of i from xs
--i must same type as a in xs :: [a]
--returns list without instances of i
rem' i xs = [ x | x <- xs, not (i == x)]

--returns a list of the free variables of expression e
--from adaption of oz code from class
freeVars e = case e of Apply f a -> freeVars f ++ freeVars a
                       Lambda (Atom v) e -> rem' v (freeVars e)
                       Atom v -> [v]

--returns list of all variables of expression e
--[a] where a :: Char
allVars e = case e of Apply f a -> (allVars f) ++ (allVars a)
                      Lambda (Atom v) e -> (allVars e) ++ [v]
                      Atom v -> [v]

--Grabs the first letter in the difference between the set of all lowercase letters and set of used letters
--Returns string of a letter
pickFreshVar l =  head $ toList $ difference (alphaSet) (fromList l)

isRedex e = case e of Lambda (Atom v) e -> (elem v (freeVars e))
                      Apply f a -> (isRedex f) && (isRedex a)
                      Atom v -> False

------------ALPHA RENAMING-----------------------------

--replaces all x with a new name for alpha renaming
--returns expression with new name in place of that for string x
alphaReplace x e = case e of Lambda v se -> Lambda (alphaReplace x v) (alphaReplace x se)
                             Apply f a -> Apply (alphaReplace x f) (alphaReplace x a)
                             Atom v -> if v == x then Atom (pickFreshVar (allVars e)) else e
--renames e
--takes expression and list of free vars in that expression, alpha renames all in that list
rename e l = case l of [] -> e
                       h:t -> rename (alphaReplace h e) t

--does alpha renaming recursion
--important to note that it pays attention to the free variables closely
alphaRenaming e = case e of Apply f a -> Apply (alphaRenaming (rename f (freeVars a))) a
                            Lambda v se -> Lambda v (alphaRenaming se)
                            v1@(Atom v) -> v1


------------BETA REDUCTION-----------------------------

--Wrapper that checks to see if we're done reducing
--takes expression and eventually returns fully reduced expression
betaReduction e = if e /= x then betaReduction x else e
  where x = br' e 

--replaces occurrences of x in expression e with z, the  grunt work of beta reduction
--returns expression e
betaReplace x e z = case e of Lambda v se -> Lambda v (betaReplace x se z)
                              Apply f a -> Apply (betaReplace x f z) (betaReplace x a z)
                              atomE -> if atomE == x then z else e

--beta reduction recursion definitions
--first line is the meat of the recursion.
br' e = case e of Apply (Lambda av@(Atom v) e2) e3 -> br' (betaReplace av (br' e2) (br' e3))
                  Apply e1 e2 -> Apply (br' e1) (br' e2)
                  Lambda v se -> Lambda (br' v) (br' se)
                  (Atom v) -> (Atom v)


-------------ETA REDUCTION----------------------------

--Wrapper that checks to see if we're done eta reducing
etaReduction e = if e /= x then etaReduction x else e
  where x = er' e

--performs eta reduction recurision
er' e = case e of Apply e1 e2 -> Apply (er' e1) (er' e2)
                  Lambda (Atom v) (Apply e1 (Atom v1)) -> if ((v == v1) && (elem  v (freeVars e1))) then Lambda (Atom v) (Apply (er' e1) (Atom v)) else e1
                  Lambda e1 e2 -> Lambda (er' e1) (er' e2) 
                  v1@(Atom v) -> v1


------------REDUCER------------------------------------

--Does alpha renaming FIRST, then applies beta reduction, then applies eta reduction
reduce' lexp =  etaReduction $ betaReduction $ alphaRenaming lexp



------------MAIN METHOD---------------------------------

-- Entry point of program
main = do
    putStrLn "Please enter a filename containing lambda expressions:"
    fileName <- getLine
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram fileName reduce'
