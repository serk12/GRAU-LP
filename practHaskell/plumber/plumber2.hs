import System.IO
import System.Random
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
data Val a = Join a | Pipe (a,[a]) | Vector a [Val a] | Leet a | Undefined     deriving (Read, Show)
data MemoryTree a = Null | Node String (Val a) (MemoryTree a) (MemoryTree a)  deriving (Read, Show) 
data MemoryList a = List [(String, Val a)]                                    deriving (Read, Show)

class SymTable m where
    update::m a->String->Val a->m a 
    value:: m a->String->Maybe (Val a)
    start:: m a

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

popVector::Maybe (Val a)->Either (Val a) String
popVector (Just (Vector _ v))
  | null v        = Right "empty vector"
  | otherwise     = Left (head v)  
popVector Nothing = Right "undefined variable"
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
pushVector _ _        = Right "type error"

part2::(Num a,Ord a)=>a->a
part2 a = aux (a-1)
  where aux x
          | (x+x) <= a  = x
          | otherwise   = aux (x-1)
          
splitPipe::(Num a,Ord a)=>Maybe(Val a)->Either (Val a,Val a) String
splitPipe (Just (Pipe (d,v))) = let (p1,p2) = splitAt mid v
                             in if cut1 == 0 
                                   then Left (Pipe (d,p1), Pipe (d,p2))
                                   else Left (Pipe (d,(init p1)++[cut1]), Pipe (d,cut2:p2))
  where (mid,cut1,cut2) = splitAux (part2 $ sum v) v
        splitAux d (x:xs)
          | x >= d     = (1,x - d, d)
          | otherwise = let (a,b,c) = splitAux (d-x) xs
                        in (a+1,b,c)

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
  | isJust result = Right $ "type error"
  | otherwise     = Right $ "undefined variable"
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
evalNum m (Diameter a)  = keepError (\x-> fst (fromPipe x)) (valueErr (isPipe) m a)
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

mergeTube::(Ord a)=>Either (Val a) String->Either (Val a) String->Either (Val a) String->Either (Val a) String
mergeTube _ _ (Right a) = Right a
mergeTube _ (Right a) _ = Right a
mergeTube (Right a) _ _ = Right a
mergeTube (Left (Pipe (l1,p1))) (Left (Pipe (l2,p2))) (Left (Join l3))
  | l1 == l2 && l2 == l3  = Left $ Pipe (l1,(p1++p2))
  | otherwise   = Right "unmatched diameter"
mergeTube _ _ _ = Right "error type"

evalPipe::(Num a, Ord a, SymTable m)=>m a->TExpr a->Either (Val a) String
evalPipe m (TVar a)      = valueErr (isPipe) m a
evalPipe m (Merge a c b) = mergeTube (evalPipe m a) (evalPipe m b) (evalJoin m c)
evalPipe m (Tube a b)    = keepError2 (\x y->Pipe (x,[y])) (evalNum m a) (evalNum m b)

takeAndRepeat::(Num a,Ord a)=>a->a->a
takeAndRepeat a b
  | a == 0    = 0
  | otherwise = (b + takeAndRepeat (a-1) b)*10
printPippe::(Num a,Ord a)=>Val a->[a]
printPippe (Pipe (a,x)) = foldl (\y z->(takeAndRepeat z a)) 0 x
  
  
interpretCommand::(Num a, Ord a, SymTable m)=> m a->[a]->Command a->((Either String [a]), m a, [a])
interpretCommand mem input (Seq []) = (Right [],mem,input)
interpretCommand mem input (Seq (x:xs))
  | isLeft result     = (result, memUpdate, inputUpdaet)
  | isLeft resultRest = (resultRest, memRest, inputRest)
  | otherwise         = (Right $ fromRight result ++ fromRight resultRest, memRest, inputRest)
    where (result,memUpdate,inputUpdaet)  = interpretCommand mem input x
          (resultRest, memRest,inputRest) = interpretCommand memUpdate inputUpdaet $ Seq xs
interpretCommand mem input (Pop vec var)
  | isLeft result = (Right [], update mem var $ fromLeft result, input)
  | otherwise     = (Left $ fromRight result, mem, input)
  where result = popVector $ value mem vec
interpretCommand mem input (Push vec var)
  | isLeft result = (Right [], update mem vec $ fromLeft result, input)
  | otherwise     = (Left $ fromRight result, mem, input)
  where result = pushVector (value mem vec) $ value mem var
interpretCommand mem input (Split str1 str2 cut)
  | isLeft result = let (div1,div2) = fromLeft result
                    in (Right [], update (update mem str1 div1) str2 div2, input)
  | otherwise     = (Left $ fromRight result, mem, input)
  where result = splitPipe $ value mem cut
interpretCommand mem (x:xs) (Input str) = (Right [], update mem str $ Leet x, xs)
interpretCommand mem input (Copy str1 str2)
  | isJust result = (Right [], update mem str1 $ fromJust result, input)
  | otherwise     = (Left "undefined variable", mem, input) 
  where result = value mem str2
interpretCommand mem input (Loop boolExp code)
  | isLeft result && fromLeft result = (Right [],mem,input)
  | isLeft result = interpretCommand mem input code
  | otherwise     = (Left $ fromRight result, mem, input) 
  where result = evalBool mem boolExp
interpretCommand mem input (Cond boolExp code1 code2)
  | isLeft result && fromLeft result = interpretCommand mem input code1
  | isLeft result = interpretCommand mem input code2
  | otherwise     = (Left $ fromRight result, mem, input)
  where result = evalBool mem boolExp
interpretCommand mem input (Print expr) = (flipEither $ keepError (:[]) (evalNum   mem expr), mem, input)
interpretCommand mem input (Draw  expr) = (flipEither $ keepError (printPippe) (evalPipe mem expr), mem, input)
interpretCommand mem input (TAssign str expr)
  | isLeft result = (Right [], update mem str $ fromLeft result, input)
  | otherwise     = (Left $ fromRight result, mem, input) 
  where result = evalPipe mem expr
interpretCommand mem input (CAssign str expr)
  | isLeft result = (Right [], update mem str $ fromLeft result, input)
  | otherwise     = (Left $ fromRight result, mem, input) 
  where result = evalJoin mem expr
interpretCommand mem input (DeclareVector str expr)
  | isLeft result = (Right [], update mem str (Vector (fromLeft result) []), input) 
  | otherwise     = (Left $ fromRight result, mem, input) 
  where result = evalNum mem expr


-- -- -- -- -- -- PROGRAM EXE -- -- -- -- -- --
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
  putStr "End\n"
  