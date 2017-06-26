import Data.Maybe
import Data.Either
import Unsafe.Coerce
import Data.List

import System.IO
import System.Random

showSeq::[Command a] -> String-> String
showSeq [] _   = ""
showSeq ((Cond (a,(Seq b, Seq c))):xs) k  = "\n" ++ k ++ "Cond (" ++ (show a) ++ ")\n" ++ k ++ "  Seq" ++ (showSeq b (k ++ "    ")) ++ "\n" ++ k ++ "  Seq" ++ (showSeq c (k ++ "    ")) ++ (showSeq xs k)
showSeq ((Loop (a, Seq b)):xs)         k  = "\n" ++ k ++ "Loop (" ++ (show a) ++ ")\n" ++ k ++ "  Seq" ++ (showSeq b (k ++ "    ")) ++ (showSeq xs k)
showSeq (x:xs)                         k  = "\n" ++ k ++ (show x) ++ (showSeq xs k)

instance Show (Command a) where
    show (Seq a)                        = "Seq"      ++ showSeq a "  "
    show (Input a)                      = "Input ("  ++ (show a) ++ ")"
    show (Print a)                      = "Print ("  ++ (show a) ++ ")"
    show (Empty a)                      = "Empty ("  ++ (show a) ++ ")"
    show (Size (a,b))                   = "Size ("   ++ (show a) ++ " " ++ (show b) ++ ")"
    show (Push (a,b))                   = "Push ("   ++ (show a) ++ " " ++ (show b) ++ ")"
    show (Pop (a,b))                    = "Pop ("    ++ (show a) ++ " " ++ (show b) ++ ")"
    show (Assign (a,b))                 = "Assign (" ++ (show a) ++ " " ++ (show b) ++ ")"
    show (Loop (a, Seq b))              = "Loop ("   ++ (show a) ++ ")\n  Seq" ++ (showSeq b "    ")
    show (Cond (a,(Seq b, Seq c)))      = "Cond ("   ++ (show a) ++ ")\n  Seq" ++ (showSeq b "    ") ++"\n  Seq" ++ (showSeq c "    ")
                 
instance Show (NExpr a) where
    show (Plus (a,b))       = "Plus ("  ++ (show a) ++ " " ++ (show b) ++ ")"  
    show (Minus (a,b))      = "Minus (" ++ (show a) ++ " " ++ (show b) ++ ")"
    show (Times (a,b))      = "Times (" ++ (show a) ++ " " ++ (show b) ++ ")"
    show (Var a)            = "Var "    ++  show a
    show (Const a)          = "Const "  ++  show (toInteger (unsafeCoerce a))
    
instance Show a => Show (BExpr a) where
    show (Eq (a,b))         = "Eq ("  ++ (show a) ++ " " ++ (show b) ++ ")" 
    show (AND (a,b))        = "AND (" ++ (show a) ++ " " ++ (show b) ++ ")" 
    show (Gt (a,b))         = "Gt ("  ++ (show a) ++ " " ++ (show b) ++ ")" 
    show (OR (a,b))         = "OR ("  ++ (show a) ++ " " ++ (show b) ++ ")" 
    show (NOT a)            = "NOT "  ++ (show a)

data BExpr a = Eq   (NExpr a, NExpr a) | AND   (BExpr a, BExpr a) | Gt    (NExpr a, NExpr a) | OR (BExpr a, BExpr a) | NOT (BExpr a) deriving (Read)
data NExpr a = Plus (NExpr a, NExpr a) | Minus (NExpr a, NExpr a) | Times (NExpr a, NExpr a) | Var Ident             | Const a deriving (Read)

data Command a = Seq    [Command a]                         |
                 Input  (NExpr a)                           |
                 Print  (NExpr a)                           |
                 Empty  (NExpr a)                           |
                 Size   (NExpr a , NExpr a)                 |
                 Push   (NExpr a , NExpr a)                 |
                 Pop    (NExpr a , NExpr a)                 |
                 Assign (NExpr a , NExpr a)                 |
                 Loop   (BExpr a , Command a)               |
                 Cond   (BExpr a, (Command a, Command a))   |
                 Null
                 deriving (Read)

                 --Key of the Map so this way we have log time finding and no variables repeated 
type Ident       = String
data SymbTable a = Leet Ident a (SymbTable a) (SymbTable a) | Array Ident [a] (SymbTable a) (SymbTable a) | End deriving (Show, Read)
                ---1337       ONLY ONE VAR                     ONLY PILES      

getType:: (Num a, Ord a) => SymbTable a -> Ident -> String    
getType table key  = fst (findNode table key) 

getValue:: (Num a, Ord a) => SymbTable a -> Ident -> Maybe a    
getValue table key 
  | isRight aux     = fromRight aux
  | otherwise       = Just $ head $ fromJust(fromLeft aux)
    where aux       = snd (findNode table key)

getValueArr:: (Num a, Ord a) =>  SymbTable a -> Ident -> Maybe [a]    
getValueArr table key 
  | isRight aux     = Just [fromJust (fromRight aux)]
  | otherwise       = fromLeft aux
    where aux       = snd (findNode table key)

addNewMake:: SymbTable a -> Ident -> Either a [a] -> SymbTable a --ToDo: (or not) valancear el arbol 
addNewMake (Leet ident val x y) key value
  | ident > key                     = Leet ident val (addNewMake x key value) y
  | ident < key                     = Leet ident val x (addNewMake y key value)
  | otherwise                       = Leet ident (fromLeft value) x y               -- NEW VAR
addNewMake (Array ident val x y) key value
  | ident > key                     = Array ident val (addNewMake x key value) y
  | ident < key                     = Array ident val x (addNewMake y key value)
  | isLeft value                    = Array ident ([(fromLeft value)] ++ val) x y   -- PUSH CASE
  | (length (fromRight value)) == 0 = Array ident [] x y                            -- EMPTY CASE
  | (length (fromRight value)) >  0 = Array ident (fromRight value) x y             -- POP CASE
addNewMake End key value
  | isLeft value                    = Leet key (fromLeft value) End End
  | (length (fromRight value)) == 0 = Array key [] End End                          -- NEW ARRAY
  | (length (fromRight value)) > 0  = Array key (fromRight value) End End           -- NEW VAR
  
findNode:: SymbTable a -> Ident -> (String, (Either (Maybe [a]) (Maybe a)))
findNode End key         = ("Error", Right Nothing)
findNode (Leet name val x y) key 
  | name == key         = ("Leet", Right (Just val))
  | name > key          = findNode x key
  | otherwise           = findNode y key
findNode (Array name val x y) key 
  | name == key         = ("Array", Left (Just val))
  | name > key          = findNode x key
  | otherwise           = findNode y key

class Evaluable e where
    eval:: (Num a, Ord a) => (Ident -> Maybe a) -> (e a) -> (Either String a)
    typeCheck:: (Ident -> String) -> (e a) -> Bool
    evalAux:: (Num a, Ord a) => (Ident -> Maybe a) -> (a -> a -> a) -> (e a,e a) -> (Either String a)
    evalAux f op x
      | (isLeft res1) && (isLeft res2)  = Left ((fromLeft res1) ++ (fromLeft res2))
      | isLeft res1                     = res1
      | isLeft res2                     = res2
      | otherwise                       = Right (op (fromRight res1) (fromRight res2))
        where
            res1 = eval f (fst x)
            res2 = eval f (snd x)
            
instance Evaluable NExpr where
    eval f (Plus  x) = evalAux f (+) x  
    eval f (Minus x) = evalAux f (-) x
    eval f (Times x) = evalAux f (*) x
    eval _ (Const x) = Right x
    eval f (Var x)
      | res == Nothing          = Left ("Error var" ++ x ++ " don't exists")
      | otherwise               = Right (fromJust res)
        where res = f x
    typeCheck f (Plus  x) = typeCheck f (fst x) && typeCheck f (snd x)
    typeCheck f (Minus x) = typeCheck f (fst x) && typeCheck f (snd x)
    typeCheck f (Times x) = typeCheck f (fst x) && typeCheck f (snd x)
    typeCheck f (Const x)  = True
    typeCheck f (Var x)    = (f x) == "Leet"
    
instance Evaluable BExpr where
    eval f (Eq x)  = evalAux f (intBoolRe (==)) x
    eval f (Gt x)  = evalAux f (intBoolRe (> )) x
    eval f (AND x) = evalAux f (intBoolOp (&&)) x
    eval f (OR x)  = evalAux f (intBoolOp (||)) x
    eval f (NOT x)
      | isLeft l                        = l
      | (head (rights [l])) == 1        = Right 0
      | otherwise                       = Right 1
        where l = eval f x
    typeCheck f (Eq x)  = typeCheck f (fst x) && typeCheck f (snd x)
    typeCheck f (Gt x)  = typeCheck f (fst x) && typeCheck f (snd x)
    typeCheck f (AND x) = typeCheck f (fst x) && typeCheck f (snd x)
    typeCheck f (OR x)  = typeCheck f (fst x) && typeCheck f (snd x)
    typeCheck f (NOT x) = typeCheck f x

intBoolRe:: (Num a, Ord a) => (a -> a -> Bool) -> a -> a -> a    
intBoolRe op x y
  | op x y      = 1
  | otherwise   = 0
intBoolOp:: (Num a, Ord a) => (Bool -> Bool -> Bool) -> a -> a -> a
intBoolOp op x y
  | (op (toBool x) (toBool y))              = 1
  | otherwise                               = 0
    where toBool::(Num a, Ord a) =>  a -> Bool
          toBool x
            | x == 0    = False
            | otherwise = True

fromRight:: Either a b -> b
fromRight (Right b) = b
fromLeft:: Either a b -> a
fromLeft  (Left  a) = a

interpretProgram::(Num a, Ord a) => [a] -> Command a -> (Either String [a])
interpretProgram input code = aux
    where (aux,_,_) = interpretCommand End input code
                                 --estat memoria->input-> codi 4 exe->   L|R    errors out , memory state, restOfImput
interpretCommand::(Num a, Ord a) => SymbTable a -> [a] -> Command a -> ((Either String [a]), SymbTable a, [a])
interpretCommand variables input (Seq x)
  | null x                  = (Right [], variables, input)
  | length x == 1           = (eit, var, inp)
  | isLeft eit              = (eit, var, inp)
  | isLeft eit2             = (eit2, var2, inp2)
  | otherwise               = (Right ((fromRight eit) ++ (fromRight eit2)), var2, inp2)
    where (eit,  var,  inp) = interpretCommand variables input (head x)
          (eit2, var2,inp2) = interpretCommand var inp (Seq (tail x))
interpretCommand variables input (Input (Var x)) 
  | (getType variables x) == "Array"    = (Left ("type error" ++ x), variables, input)
  | null input                          = (Left "ok you broke my code nice move ); how dare you?", variables, input)
  | otherwise                           = (Right [], addNewMake variables x (Left (head input)), tail input)
interpretCommand variables input (Print (Var x))
  | (getType variables x) == "Array"    = (Left ("undefined variable " ++ x), variables, input)
  | otherwise                           = (Right [fromJust (getValue variables x)], variables, input)
interpretCommand variables input (Empty (Var x)) = ( Right [], addNewMake variables x (Right []), input)
interpretCommand variables input (Size ((Var y), (Var x)))
  | ((getType variables x) /= "Array") && ((getType variables y) == "Array") = (Right [], addNewMake variables x (Left (fromIntegral (length (fromJust (getValueArr variables y))))), input)
  | otherwise                                                               = (Left ("type error in: " ++ x ++ " or " ++ y), variables, input)
interpretCommand variables input (Push ((Var x), y))
  | (typeCheck (getType variables) y) && ((getType variables x) /= "Leet")  = (Right [], addNewMake variables x (Left (fromRight (eval (getValue variables) y))), input)
  | otherwise                                                               = (Left ("type error"), variables, input)
interpretCommand variables input (Pop (Var x, Var y))
  | ((getType variables y) /= "Array") && ((getType variables x) == "Array") && (length aux > 0)  = (Right [], addNewMake (addNewMake variables y (Left (head aux))) x (Right (tail aux)), input)
  | otherwise                                                                                     = (Left ("type error or empty stack"), variables, input)
    where aux = fromJust (getValueArr variables x) 
interpretCommand variables input (Assign (Var x, y))
  | (typeCheck (getType variables) y) && ((getType variables x) /= "Array") = (Right [], addNewMake variables x (Left (fromRight (eval (getValue variables) y))), input)
  | otherwise                                                               = (Left ("type error"), variables, input)
interpretCommand variables input (Loop (x,y))
  | not (typeCheck (getType variables) x)           = (Left("type error"), variables, input)
  | (fromRight (eval (getValue variables) x)) == 1  = result
  | otherwise                                       = (Right [], variables, input)
    where (a,var2,inp2) = interpretCommand variables input y
          result        = interpretCommand var2 inp2 (Loop (x,y))
interpretCommand variables input (Cond (x,(y,z)))
  | not (typeCheck (getType variables) x)           = (Left("type error"), variables, input)
  | (fromRight (eval (getValue variables) x)) == 1  = interpretCommand variables input y
  | otherwise                                       = interpretCommand variables input z   

readInt::String -> Integer
readInt a = (read a::Integer)

readFloat::String -> Float
readFloat a = (read a::Float)

readInputs = do
    input <- getLine
    if input == ""
       then return []
       else do 
        l <- readInputs
        return (input:l)

infiniteOfRandom:: IO [Integer]
infiniteOfRandom = do
                g <- newStdGen
                return $ randomRs (-99,99) g
infiniteOfRandomF::IO [Float]
infiniteOfRandomF = do
                g <- newStdGen
                return $ randomRs (-99,99) g

getUsed::(Num a, Ord a) => [a] -> [a] -> [a]
getUsed a b
  | (length used) == 100    = getUsedExtensed a b 10000
  | otherwise               = used
    where used = ((take 100 a) \\ (take 100 b))
          getUsedExtensed::(Num a, Ord a) => [a] -> [a] -> Int -> [a]
          getUsedExtensed a b c
            | (length used2) == c   = getUsedExtensed a b (c*10)
            | otherwise             = used2
              where used2 = ((take c a) \\ (take c b))
                    
runKProgram:: (Num a, Ord a) => Integer -> [a] -> Command a -> [(Either String [a], [a])]
runKProgram 0 _ _ = []
runKProgram k infini code = (result, getUsed infini infini2) : (runKProgram (k - 1) (infini2) code)
    where (result,_,infini2) = interpretCommand End infini code

cleanPrint a = putStr (" " ++ (show a))
printResult a = 
    case a of
         (Left a)  -> do 
                    putStr (a ++ "\n")
         (Right a) -> do 
                    mapM_ (cleanPrint) a 
                    putStr "\n"
printPair (a,b) = do
                putStr "Outputs:"
                printResult a
                putStr "Inputs:"
                printResult (Right b)
                putStr "\n"
printResultPair a = mapM_ (printPair) a

main = do
    file <- openFile "./programhs.txt" ReadMode
    code <- hGetLine file
    hClose file
    putStr "do you work with Integers (0) or Reals (1)\n"
    typeVar <- getLine
    case typeVar of
         "0"        -> do
                    putStr "What test do you whant:\n0: Manual execution\n1: Unic test\n2: Multiple test\n"
                    typeTest <- getLine
                    infini <- infiniteOfRandom
                    case typeTest of
                        "0"    -> do
                                inputs <- readInputs
                                printResult $ interpretProgram ((map readInt inputs) ++ infini) (read code ::(Command Integer))
                        "1"    -> printResultPair $ runKProgram 1 infini (read code ::(Command Integer))
                        "2"    -> do 
                                k <- getLine
                                printResultPair $ runKProgram (read k :: Integer) infini (read code ::(Command Integer))
                        otherwise-> main
         "1"        -> do
                    putStr "What test do you whant:\n0: Manual execution\n1: Unic test\n2: Multiple test\n"
                    typeTest <- getLine
                    infini <- infiniteOfRandomF
                    case typeTest of
                        "0"    -> do
                                inputs <- readInputs
                                printResult $ interpretProgram ((map readFloat inputs) ++ infini) (read code ::(Command Float))
                        "1"    -> printResultPair $ runKProgram 1 infini (read code ::(Command Float))
                        "2"    -> do 
                                k <- getLine
                                printResultPair $ runKProgram (read k :: Integer) infini (read code ::(Command Float))
                        otherwise-> main
         otherwise  -> main
----------- 300 LINES OF HASKELL ACHIVEMENT UNLOCKED https://jutge.org/awards/languages/Haskell.png --------------------------------