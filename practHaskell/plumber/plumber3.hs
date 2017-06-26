import System.IO
import System.Random

import Debug.Trace
-- -- -- -- -- -- PROGRAM GRM -- -- -- -- -- --
showSeq::(Show a) => [Command a]-> String-> String
showSeq [] _ = ""
showSeq ((Cond a (Seq b) (Seq c)):xs) k = "\n" ++ k ++ "if(" ++ (show a) ++ ")" ++ 
                                           " {" ++ (showSeq b (k ++ "  ")) ++ "\n" ++ k ++ "}\n" ++ 
                                           k ++ "else {" ++ (showSeq c (k ++ "  ")) ++ "\n" ++ k ++ "}" ++ 
                                           (showSeq xs k) 
showSeq ((Loop a (Seq b)):xs)         k = "\n" ++ k ++ "while(" ++ (show a) ++ ") {" ++ 
                                            (showSeq b (k ++ "  "))  ++ "\n" ++ k ++ "}" ++ 
                                            (showSeq xs k)
showSeq (x:xs) k = "\n" ++ k ++ (show x) ++ k ++ (showSeq xs k)

printFun::(Show a) => String->[a]->String
printFun a b = a ++ "(" ++ (foldl (\x y -> x ++ ", " ++ (show y)) (show (head b)) (tail b)) ++ ")"

printOp::(Show a,Show b) => String->a->b->String
printOp op a b = (show a) ++ " " ++ op ++ " " ++ (show b)

printPriorityOp::(Show a, Show b) => String->a->b->String
printPriorityOp op a b = "(" ++ (printOp op a b) ++ ")"

instance Show a => Show (Command a) where
  show (Seq a)                = showSeq a "  "
  show (Input a)              = printFun "Input" [a]
  show (Print a)              = printFun "Print" [a]
  show (Draw a)               = printFun "Draw"  [a]
  show (Push a b)             = printFun "Push"  [a,b]
  show (Pop  a b)             = printFun "Pop"   [a,b]
  show (Copy a b)             = printFun "Copy"  [a,b]
  show (Split a b c)          = printFun "Split" [a,b,c]
  show (Loop a b)             = showSeq [(Loop a b  )] ""
  show (Cond a b c)           = showSeq [(Cond a b c)] "" 
  show (TAssign a b)          = "Tube "      ++ (printOp "=" a b)
  show (CAssign a b)          = "Connector " ++ (printOp "=" a b)
  show (DeclareVector a b)    = "Vector "    ++ (show a) ++ "[" ++ (show b) ++ "]"

instance Show a => Show (NExpr a) where
  show (Var a)        = show a
  show (Const a)      = show a
  show (Length a)     = printFun "Length" [a]
  show (Diameter a)   = printFun "Diameter" [a]
  show (Times a b)    = printPriorityOp "*" a b
  show (Plus a b)     = printPriorityOp "+" a b  
  show (Minus a b)    = printPriorityOp "-" a b
  
instance Show a => Show (BExpr a) where
  show (Not a)    = "!"  ++ (show a)
  show (Full a)   = printFun "Full" [a]
  show (Empty a)  = printFun "Empty" [a]
  show (And a b)  = printPriorityOp "&" a b
  show (Gt a b)   = printPriorityOp ">" a b
  show (Lt a b)   = printPriorityOp "<" a b
  show (Or a b)   = printPriorityOp "|" a b
  show (Eq a b)   = printPriorityOp "==" a b

instance Show a => Show (TExpr a) where
  show (TVar a)       = show a
  show (Tube a b)     = printFun "Tube" [a,b]
  show (Merge a b c)  = "Merge(" ++ (show a) ++ ", " ++ (show b) ++ ", " ++ (show c) ++ ")"
  
instance Show a => Show (CExpr a) where
  show (CVar a)       = show a
  show (Connector a)  = printFun "Connector" [a]
    
type Ident  = String 

data BExpr a = Eq (NExpr a) (NExpr a)   | And (BExpr a) (BExpr a)   | Gt  (NExpr a) (NExpr a)   | 
               Lt (NExpr a) (NExpr a)   | Or  (BExpr a) (BExpr a)   | Not (BExpr a)             | 
               Empty Ident              | Full Ident                                              deriving (Read)
data NExpr a = Plus (NExpr a) (NExpr a) | Minus (NExpr a) (NExpr a) | Times (NExpr a) (NExpr a) | 
               Var Ident                | Length Ident              | Diameter Ident            | 
               Const a                                                                            deriving (Read)

data TExpr a = TVar Ident | Merge (TExpr a) (CExpr a) (TExpr a) | Tube (NExpr a) (NExpr a)        deriving (Read)
data CExpr a = CVar Ident | Connector (NExpr a)                                                   deriving (Read)

data Command a = Seq [Command a]                        
                 | Copy Ident Ident                       
                 | TAssign Ident (TExpr a)                
                 | CAssign Ident (CExpr a)                
                 | Split Ident Ident Ident                
                 | Print (NExpr a)                        
                 | Input Ident
                 | Draw (TExpr a)                
                 | Loop (BExpr a) (Command a)       
                 | Cond (BExpr a) (Command a) (Command a) 
                 | DeclareVector Ident (NExpr a)          
                 | Push Ident Ident                       
                 | Pop Ident Ident
                 deriving (Read)

-- -- -- -- -- -- PROGRAM MEM -- -- -- -- -- --
data Val a = Join a | Pipe (a,[a]) | Vector a [Val a] | Leet a | Undefined    deriving (Show,Eq)
data MemoryTree a = Null | Node String (Val a) (MemoryTree a) (MemoryTree a)  deriving (Show) 
data MemoryList a = List [(String, Val a)]                                    deriving (Show)

class SymTable m where
    update::m a->String->Val a->m a 
    value:: m a->String->Maybe (Val a)
    start:: m a
    comparate::(Eq a)=>m a->m a->[(String, Maybe (Val a), Maybe (Val a))]

merge::[(String,a)]->(String,a)->[(String,a)]
merge [] x = [x]
merge (y:ys) (var,val)
  | var > (fst y) = y:(merge ys (var,val))
  | var < (fst y) = (var,val):(y:ys)
  | otherwise     = (var,val):ys

instance SymTable (MemoryList) where
  start = List []
  update (List memory) var val = List $ merge memory (var, val)
  value  (List []) _  = Nothing
  value  (List (x:xs)) var
    | (fst x) == var  = Just $ snd x
    | otherwise       = value (List xs) var
  comparate (List []) (List []) = []
  comparate (List []) (List ((a,b):xs)) = (a,Nothing,Just b):(comparate (List []) (List xs))
  comparate (List ((a,b):xs)) (List []) = (a,Just b,Nothing):(comparate (List xs) (List []))
  comparate (List ((a,b):xs)) (List ((c,d):ys))
    | a > c     = (c,Nothing, Just d):(comparate (List ((a,b):xs)) (List ys))
    | a < c     = (a,Just b, Nothing):(comparate (List xs) (List ((c,d):ys)))
    | b /= d    = (a,Just b, Just d):(comparate (List xs) (List ys))
    | otherwise = comparate (List xs) (List ys)

instance SymTable (MemoryTree) where
  start = Null
  update Null var val = Node var val Null Null
  update (Node name info x y) var val
    | var > name  = Node name info x (update y var val)
    | var < name  = Node name info (update x var val) y
    | otherwise   = Node name val x y
  value Null _ = Nothing
  value (Node name val x y) var
    | var > name  = value y var
    | var < name  = value x var
    | otherwise   = Just val
  comparate Null Null = []
  comparate (Node name info x y) Null = (comparate x Null) ++ [(name,Just info,Nothing)] ++ (comparate y Null)
  comparate Null (Node name info x y) = (comparate Null x) ++ [(name,Nothing,Just info)] ++ (comparate Null y)
  comparate (Node name1 info1 x y) (Node name2 info2 v w) 
    | name1 > name2   = (comparate x (Node name2 info2 v w)) ++ [(name1,Just info1,Nothing)] ++ (comparate y Null) 
    | name1 < name2   = (comparate (Node name1 info1 x y) v) ++ [(name2,Nothing,  Just info2)] ++ (comparate Null w)
    | info1 /= info2  = (comparate x v) ++ [(name1, Just info1, Just info2)] ++ (comparate y w)
    | otherwise       = (comparate x v) ++ (comparate y w)
 
  
-- -- -- -- -- -- PROGRAM CODE -- -- -- -- -- --
fromRight:: Either a b -> b
fromRight (Right b) = b
fromLeft:: Either a b -> a
fromLeft  (Left  a) = a
isRight::Either a b -> Bool
isRight (Right _) = True
isRight _         = False
isLeft::Either a b -> Bool
isLeft a = not $ isRight a 

fromJust::Maybe a->a
fromJust (Just a) = a
fromJust Nothing = error "Maybe.fromJust: Nothing"
isJust::Maybe a->Bool
isJust (Just _) = True
isJust _        = False
isNothing::Maybe a->Bool
isNothing a = not $ isJust a


isUndefined::Val a->Bool
isUndefined Undefined = True
isUndefined _         = False
isJoin::Val a->Bool
isJoin (Join _) = True
isJoin _        = False
isLeet::Val a->Bool
isLeet (Leet _) = True
isLeet _        = False
isPipe::Val a->Bool
isPipe (Pipe _) = True
isPipe _        = False
isVector::Val a->Bool
isVector (Vector _ _) = True
isVector _            = False
fromVector::Val a->(a,[Val a])
fromVector (Vector a b) = (a,b)
fromVector _            = error "Val.fromVector: Not Vector"
fromLeet::Val a->a
fromLeet (Leet a) = a
fromLeet _        = error "Val.fromLeet: Not Leet"
fromPipe::Val a->(a,[a])
fromPipe (Pipe a) = a
fromPipe _        = error "Val.fromPipe: Not Pipe"
getDiameter::Val a->a
getDiameter (Pipe (a,_))  = a
getDiameter (Join a)      = a
getDiameter _             = error "Val.getDiameter: Not Pipe or Join"

popVector::Maybe (Val a)->Either (a,[Val a]) String
popVector (Just (Vector a v))
  | null v        = Right "empty vector"
  | otherwise     = Left (a,v)
popVector Nothing = Right "undefined variable"
popVector (Just Undefined) = Right "variable content no longer exists"
popVector _       = Right "type error"

lengthEq::(Ord b, Num b)=>[a]->b->Bool
lengthEq [] a = a == 0
lengthEq (x:xs) a = lengthEq xs (a-1)

pushVector::(Ord a, Num a)=>Maybe(Val a)->Maybe (Val a)->Either (Val a) String
pushVector (Just (Vector x v)) (Just (Pipe p))
  | lengthEq v x      = Right "full vector"
  | otherwise         = Left (Vector x ((Pipe p):v))
pushVector _ Nothing  = Right "undefined variable"
pushVector Nothing _  = Right "undefined variable"
pushVector (Just Undefined) _ = Right "variable content no longer exists"
pushVector _ (Just Undefined) = Right "variable content no longer exists"
pushVector _ _        = Right "type error"

part2::(Num a,Ord a)=>a->a
part2 a = aux (a-1)
  where aux x
          | (x+x) <= a  = x
          | otherwise   = aux (x-1)
          
splitPipe::(Num a,Ord a)=>Maybe(Val a)->Either (Val a,Val a) String
splitPipe (Just (Pipe (d,v))) = let (p1,p2) = splitAt mid v
                             in if cut2 == 0
                                   then Left (Pipe (d,p1), Pipe (d,p2))
                                   else Left (Pipe (d,(init p1)++[cut1]), Pipe (d,cut2:p2))
  where (mid,cut2,cut1) = splitAux ((sum v) - (part2 $ sum v)) v
        splitAux _ [] = (0,0,0)
        splitAux d (x:xs)
            | x >= d    = (1,x - d, d)
            | otherwise = let (a,b,c) = splitAux (d-x) xs
                          in (a+1,b,c)
splitPipe (Just Undefined) = Right "variable content no longer exists"
splitPipe Nothing = Right "undefined variable"
splitPipe _       = Right "type error"

keepError::(a->b)->Either a String->Either b String
keepError _ (Right a) = Right a
keepError op (Left a) = Left $ op a
keepError2::(a->a->b)->Either a String->Either a String->Either b String
keepError2 _ _ (Right a) = Right a
keepError2 _ (Right a) _ = Right a
keepError2 op (Left a) (Left b) = Left $ op a b
valueErr::(SymTable m)=>((Val a)->Bool)->m a->String->Either (Val a) String
valueErr op m a
  | isJust result && op (fromJust result) = Left $ (fromJust result)
  | isJust result && isUndefined (fromJust result) = Right "variable content no longer exists"
  | isJust result = Right "type error"
  | otherwise     = Right "undefined variable"
  where result = value m a

flipEither::Either a b->Either b a
flipEither (Left a)   = Right a
flipEither (Right a)  = Left a

evalNum::(Num a, Ord a, SymTable m)=>m a->NExpr a->Either a String
evalNum _ (Const a)     = Left a 
evalNum m (Var a)       = keepError (fromLeet) (valueErr (isLeet) m a)
evalNum m (Times a b)   = keepError2 (*) (evalNum m a) (evalNum m b)
evalNum m (Plus a b)    = keepError2 (+) (evalNum m a) (evalNum m b)
evalNum m (Minus a b)   = keepError2 (-) (evalNum m a) (evalNum m b)
evalNum m (Diameter a)  = keepError (\x-> getDiameter x) (valueErr (\x -> (isPipe x) || (isJoin x)) m a)
evalNum m (Length a)    = keepError (\x-> sum $ snd (fromPipe x))(valueErr (isPipe) m a)
  
evalBool::(Num a, Ord a, SymTable m)=>m a->BExpr a->Either Bool String
evalBool m (Not a)    = keepError (not) (evalBool m a)
evalBool m (And a b)  = keepError2 (&&) (evalBool m a) (evalBool m b)
evalBool m (Or a b)   = keepError2 (||) (evalBool m a) (evalBool m b)
evalBool m (Eq a b)   = keepError2 (==) (evalNum m a)  (evalNum m b)
evalBool m (Gt a b)   = keepError2 (>)  (evalNum m a)  (evalNum m b)
evalBool m (Lt a b)   = keepError2 (<)  (evalNum m a)  (evalNum m b)
evalBool m (Empty a)  = keepError (\x->null $ snd (fromVector x)) (valueErr (isVector) m a)
evalBool m (Full a)   = keepError (\x->lengthEq (snd (fromVector x)) (fst $ fromVector x)) (valueErr (isVector) m a)

evalJoin::(Num a, Ord a, SymTable m)=>m a->CExpr a->Either (Val a) String
evalJoin m (CVar a)       = valueErr (isJoin) m a
evalJoin m (Connector a)  = keepError (\x->Join x) (evalNum m a)

evalPipe::(Num a, Ord a, SymTable m)=>m a->TExpr a->Either (Val a) String
evalPipe m (TVar a)      = valueErr (isPipe) m a
evalPipe m (Merge a c b) = mergeTube (evalPipe m a) (evalPipe m b) (evalJoin m c)
evalPipe m (Tube a b)    = keepError2 (\x y->Pipe (x,[y])) (evalNum m b) (evalNum m a)

mergeTube::(Ord a)=>Either (Val a) String->Either (Val a) String->Either (Val a) String->Either (Val a) String
mergeTube _ _ (Right a) = Right a
mergeTube _ (Right a) _ = Right a
mergeTube (Right a) _ _ = Right a
mergeTube (Left (Pipe (l1,p1))) (Left (Pipe (l2,p2))) (Left (Join l3))
  | l1 == l2 && l2 == l3  = Left $ Pipe (l1,(p1++p2))
  | otherwise   = Right "unmatched diameter"
mergeTube _ _ _ = Right "error type"


takeAndRepeat::(Num a,Ord a)=>[a]->a->a->a
takeAndRepeat (x:[]) b p
  | x == 0    = 0
  | otherwise = (b*p + (takeAndRepeat [x-1] b (p*10)))
takeAndRepeat (x:xs) b p
  | x == 0    = takeAndRepeat xs b (p*10)
  | otherwise = (b*p + (takeAndRepeat ((x-1):xs) b (p*10)))
  
printPippe::(Num a,Ord a)=>Val a->a
printPippe (Pipe (d,v)) = (takeAndRepeat (reverse v) 1 1)*100000 + d -- NO log or div NO BETTER

-- evalNumKill::(Num a, Ord a, SymTable m)=>m a->NExpr a->m a
-- evalNumKill m (Const a)     = m
-- evalNumKill m (Var a)       = update m a Undefined 
-- evalNumKill m (Times a b)   = evalNumKill (evalNumKill m b) a
-- evalNumKill m (Plus a b)    = evalNumKill (evalNumKill m b) a
-- evalNumKill m (Minus a b)   = evalNumKill (evalNumKill m b) a
-- evalNumKill m (Diameter a)  = update m a Undefined 
-- evalNumKill m (Length a)    = update m a Undefined
--   
-- evalBoolKill::(Num a, Ord a, SymTable m)=>m a->BExpr a->m a
-- evalBoolKill m (Not a)    = evalBoolKill m a
-- evalBoolKill m (And a b)  = evalBoolKill (evalBoolKill m b) a
-- evalBoolKill m (Or a b)   = evalBoolKill (evalBoolKill m b) a
-- evalBoolKill m (Eq a b)   = evalNumKill  (evalNumKill m b) a
-- evalBoolKill m (Gt a b)   = evalNumKill  (evalNumKill m b) a
-- evalBoolKill m (Lt a b)   = evalNumKill  (evalNumKill m b) a
-- evalBoolKill m (Empty a)  = update m a Undefined
-- evalBoolKill m (Full a)   = update m a Undefined

evalJoinKill::(Num a, Ord a, SymTable m)=>m a->CExpr a->m a
evalJoinKill m (CVar a)       = update m a Undefined
evalJoinKill m (Connector a)  = m
-- evalJoinKill m (Connector a)  = evalNumKill m a

evalPipeKill::(Num a, Ord a, SymTable m)=>m a->TExpr a->m a
evalPipeKill m (TVar a)      = update m a Undefined
evalPipeKill m (Merge a c b) = evalPipeKill (evalPipeKill (evalJoinKill m c) b) a
evalPipeKill m (Tube a b)    = m
-- evalPipeKill m (Tube a b)    = evalNumKill (evalNumKill m b) a


interpretCommand::(Num a, Ord a, SymTable m)=> m a->[a]->Command a->((Either String [a]), m a, [a])
interpretCommand mem input (Seq []) = (Right [],mem,input)
interpretCommand mem input (Seq (x:xs))
  | isLeft result     = (result, memUpdate, inputUpdaet)
  | isLeft resultRest = (resultRest, memRest, inputRest)
  | otherwise         = (Right $ fromRight result ++ fromRight resultRest, memRest, inputRest)
    where (result,memUpdate,inputUpdaet)  = interpretCommand mem input x
          (resultRest, memRest,inputRest) = interpretCommand memUpdate inputUpdaet $ Seq xs
interpretCommand mem input (Pop vec var)
  | isLeft result = (Right [], update (update mem vec (Vector (fst $ fromLeft result)(tail $ snd $ fromLeft result))) var (head $ snd $ fromLeft result), input)
  | otherwise     = (Left $ fromRight result, mem, input)
  where result = popVector $ value mem vec
interpretCommand mem input (Push vec var)
  | isLeft result = (Right [], update (update mem var Undefined) vec $ fromLeft result, input)
  | otherwise     = (Left $ fromRight result, mem, input)
  where result = pushVector (value mem vec) $ value mem var
interpretCommand mem input (Split str1 str2 cut) 
  | isLeft result = let (div1,div2) = fromLeft result
                    in (Right [], update (update (update mem cut Undefined) str1 div1) str2 div2, input)
  | otherwise     = (Left $ fromRight result, mem, input)
  where result = splitPipe $ value mem cut
interpretCommand mem [] (Input str) = (Left "input list empty", mem, []) 
interpretCommand mem (x:xs)(Input str) = (Right [], update mem str $ Leet x, xs)
interpretCommand mem input (Print expr) = (flipEither $ keepError (:[]) (evalNum mem expr), mem, input)
interpretCommand mem input (Draw  expr) = (flipEither $ keepError (\x -> (printPippe x):[]) (evalPipe mem expr), mem, input)
interpretCommand mem input (Copy str1 str2)
  | isLeft result = (Right [], update mem str1 $ fromLeft result, input)
  | otherwise     = (Left $ fromRight result, mem, input) 
  where result = valueErr (\x -> True) mem str2
interpretCommand mem input (Loop boolExp code)
--   | isLeft result && fromLeft result = (Right [],(evalBoolKill mem boolExp),input)
--   | isLeft result = interpretCommand (evalBoolKill mem boolExp) input code
  | isLeft result && fromLeft result = let (result2,mem2,inp2) = interpretCommand mem input code
                                       in if isRight result2 && not (null $ fromRight result2)
                                             then let (result1, mem1, inp1) = interpretCommand mem2 inp2 (Loop boolExp code)
                                                  in if isLeft result1
                                                        then (result1, mem1,inp1)
                                                        else (Right ((fromRight result2)++ (fromRight result1)),mem1,inp1)
                                             else (result2,mem2,inp2) 
  | isLeft result = (Right [],mem,input)
  | otherwise     = (Left $ fromRight result, mem, input) 
  where result = evalBool mem boolExp
interpretCommand mem input (Cond boolExp code1 code2)
--   | isLeft result && fromLeft result = interpretCommand (evalBoolKill mem boolExp) input code1
--   | isLeft result = interpretCommand (evalBoolKill mem boolExp) input code2
  | isLeft result && fromLeft result = interpretCommand mem input code1
  | isLeft result = interpretCommand mem input code2
  | otherwise     = (Left $ fromRight result, mem, input)
  where result = evalBool mem boolExp
interpretCommand mem input (TAssign str expr)
  | isLeft result = (Right [], update (evalPipeKill mem expr) str $ fromLeft result, input)
  | otherwise     = (Left $ fromRight result, mem, input) 
  where result = evalPipe mem expr
interpretCommand mem input (CAssign str expr)
  | isLeft result = (Right [], update (evalJoinKill mem expr) str $ fromLeft result, input)
  | otherwise     = (Left $ fromRight result, mem, input) 
  where result = evalJoin mem expr
interpretCommand mem input (DeclareVector str expr)
--   | isLeft result = (Right [], update (evalNumKill mem expr) str (Vector (fromLeft result) []), input) 
  | isLeft result = (Right [], update mem str (Vector (fromLeft result) []), input)
  | otherwise     = (Left $ fromRight result, mem, input) 
  where result = evalNum mem expr


interpretProgram::(Num a, Ord a, SymTable m)=> m a->[a]->Command a->Either String [a]
interpretProgram m i c = aux
  where (aux,_,_) = interpretCommand m i c

-- -- -- -- -- -- PROGRAM EXE -- -- -- -- -- --
runKProgram::(Num a, Ord a, SymTable m)=>Integer->m a->[a]->Command a->[(Either String [a], [a])]
runKProgram 0 _ _ _ = []
runKProgram k m infini code = (result, []) : (runKProgram (k - 1) m (infini2) code)
    where (result,_,infini2) = interpretCommand m infini code

runAndCompareK::(Num a, Ord a, SymTable m)=>([a]->Command a->(Either String [a], m a, [a]))->Command a->Command a->[a]->Integer->[((Either String [a],m a),(Either String [a],m a))]  
runAndCompareK _ _ _ _ 0 = [] 
runAndCompareK p c1 c2 i k = ((result1,mem1),(result2,mem2)):(runAndCompareK p c1 c2 input2 (k-1))
  where (result1,mem1,input2) = p i c1
        (result2,mem2,_)      = p i c2

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
                
readInt::String -> Integer
readInt a = (read a::Integer)

readFloat::String -> Float
readFloat a = (read a::Float)
                
printResult equal [] = if equal >= 0 
                          then putStr $ "qtty test equals: " ++ (show equal) ++ "\n"
                          else putStr ""

printResult equal (((out1,mem1),(out2,mem2)):xs) = do 
  putStr $ "output code1: " ++ show out1 ++ "\noutput code2: " ++ (show out2) ++ "\n"
  let memDif = comparate mem1 mem2 
  putStr $ "diff in memory: " ++ show (memDif) ++ "\n"
  if memDif == []
     then printResult (equal+1) xs
     else printResult equal xs

main = do
  putStr "do you work with Integers (0) or Reals (1)\n"
  typeVar <- getLine
  putStr "reading files programhs1 and programhs2\n"
  file1 <- openFile "./programhs1.txt" ReadMode
  code1 <- hGetLine file1
  hClose file1
  file2 <- openFile "./programhs2.txt" ReadMode
  code2 <- hGetLine file2
  hClose file2
  putStr "memory format List (0) or BinaryTree (1)\n"
  typeMem <- getLine
  putStr "What test do you whant:\n0: Manual execution\n1: Multiple test\n"
  typeTest <-getLine
  case typeVar of
       "0" -> do
              infini <- infiniteOfRandom
              case typeTest of
                   "0" -> do
                          putStr "Inputs: \n"
                          inputs <- readInputs
                          case typeMem of
                               "0"  -> printResult (-2) $ runAndCompareK (interpretCommand (start::MemoryList Integer)) (read code1::(Command Integer)) (read code2::(Command Integer)) ((map readInt inputs) ++ infini) 1
                               "1"  -> printResult (-2) $ runAndCompareK (interpretCommand (start::MemoryTree Integer)) (read code1::(Command Integer)) (read code2::(Command Integer)) ((map readInt inputs) ++ infini) 1
                   "1" -> do
                          putStr "Qtty test? \n"
                          k <- getLine
                          case typeMem of
                               "0"  -> printResult 0 $ runAndCompareK (interpretCommand (start::MemoryList Integer)) (read code1::(Command Integer)) (read code2::(Command Integer)) infini (read k::Integer)
                               "1"  -> printResult 0 $ runAndCompareK (interpretCommand (start::MemoryTree Integer)) (read code1::(Command Integer)) (read code2::(Command Integer)) infini (read k::Integer)
       "1" -> do
              infini <- infiniteOfRandomF
              case typeTest of
                   "0" -> do
                          putStr "Inputs: "
                          inputs <- readInputs
                          case typeMem of
                               "0"  -> printResult (-2) $ runAndCompareK (interpretCommand (start::MemoryList Float)) (read code1::(Command Float)) (read code2::(Command Float)) ((map readFloat inputs) ++ infini) 1
                               "1"  -> printResult (-2) $ runAndCompareK (interpretCommand (start::MemoryTree Float)) (read code1::(Command Float)) (read code2::(Command Float)) ((map readFloat inputs) ++ infini) 1
                   "1" -> do
                          putStr "Qtty test? "
                          k <- getLine
                          case typeMem of
                               "0"  -> printResult 0 $ runAndCompareK (interpretCommand (start::MemoryList Float)) (read code1::(Command Float)) (read code2::(Command Float)) infini (read k::Integer)
                               "1"  -> printResult 0 $ runAndCompareK (interpretCommand (start::MemoryTree Float)) (read code1::(Command Float)) (read code2::(Command Float)) infini (read k::Integer)
  putStr "End\n"
  