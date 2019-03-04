import Control.Monad.State
import Data.Vector ((!),(//),Vector)
import qualified Data.Vector as V
import System.IO
import System.Environment


numOfRegisters = 16
numOfMemoryLocations = 100

newtype Reg = Reg Int deriving (Eq,Show)
type Label = String

type ASM = StateT ASMState IO

data ASMState = ASMState {programCounter :: Int,
			  instructions:: [Inst],
			  registers:: Vector Int,
			  memory :: Vector Int,
			  labels :: Label -> Int}

data Inst = Add Reg Reg Reg
	  | AddI Int Reg Reg
	  | Sub Reg Reg Reg
	  | SubI Int Reg Reg
	  | Div Reg Reg Reg
	  | DivI Int Reg Reg
	  | Mul Reg Reg Reg
	  | MulI Int Reg Reg
	  | MovI Int Reg
	  | Mov Reg Reg
          | Pc Reg
	  | Jmp Label
	  | JmpR Reg
	  | JCon Reg Label Label
	  | Print Reg
	  | And Reg Reg Reg
	  | Or Reg Reg Reg
	  | XOr Reg Reg Reg
	  | Eql Reg Reg Reg
	  | Lt Reg Reg Reg
	  | Not Reg
	  | Store Reg Reg
	  | Load Reg Reg
	  deriving (Eq, Show)

-- convention: The last register is always the one the result is stored in
-- or in the case of the Store instruction the location stored in

getReg :: Reg -> ASM Int
getReg (Reg i) = do
  rs <- gets registers
  return (rs ! i)

storeReg :: Int -> Reg -> ASM ()
storeReg v (Reg i) = do
  ASMState pc is rs m ls <- get
  put $ ASMState pc is (rs // [(i,v)]) m ls 

getMem :: Int -> ASM Int
getMem i = do
  ms <- gets memory
  return $ ms ! i

storeMem :: Int -> Int -> ASM ()
storeMem v i = do
  ASMState pc is rs m ls <- get
  put $ ASMState pc is rs (m // [(i,v)]) ls

updatePointer :: Label -> ASM ()
updatePointer lab = do
  ASMState ps is rs m ls <- get
  put $ ASMState (ls lab) is rs m ls

executeInst :: Inst -> ASM ()
executeInst (Add r1 r2 rr) = do
  res <- liftM2 (+) (getReg r1) (getReg r2)
  storeReg res rr
executeInst (AddI n r1 rr) = do
  res <- liftM2 (+) (return n) (getReg r1)
  storeReg res rr
executeInst (Sub r1 r2 rr) = do
  res <- liftM2 (-) (getReg r1) (getReg r2)
  storeReg res rr
executeInst (SubI n r1 rr) = do
  res <- liftM2 (-) (getReg r1) (return n)
  storeReg res rr
executeInst (Div r1 r2 rr) = do
  res <- liftM2 div (getReg r1) (getReg r2)
  storeReg res rr
executeInst (DivI n r1 rr) = do
  res <- liftM2 div (return n) (getReg r1)
  storeReg res rr
executeInst (Mul r1 r2 rr) = do
  res <- liftM2 (*) (getReg r1) (getReg r2)
  storeReg res rr
executeInst (MulI n r1 rr) = do
  res <- liftM2 (*) (return n) (getReg r1)
  storeReg res rr
executeInst (Mov rs rr)    = do
  res <- getReg rs
  storeReg res rr
executeInst (MovI n  rr)    = storeReg n rr
executeInst (Jmp lab)      = updatePointer lab
executeInst (JmpR r) = do
  i <- getReg r
  ASMState ps is rs m ls <- get
  put $ ASMState i is rs m ls
executeInst (Pc r) = do
  pc <- gets programCounter
  storeReg pc r
executeInst (JCon r l1 l2) = do
  res <- getReg r
  if res > 0 then updatePointer l1 else updatePointer l2
executeInst (Print r)      = do
  res <- getReg r
  liftIO $ print res
executeInst (And r1 r2 rr) = do
  b1 <- getReg r1
  b2 <- getReg r2
  if b1 > 0 && b2 > 0
    then storeReg 1 rr
    else storeReg 0 rr
executeInst (Or r1 r2 rr)  = do
  b1 <- getReg r1
  b2 <- getReg r2
  if b1 > 0 || b2 > 0
    then storeReg 1 rr
    else storeReg 0 rr
executeInst (XOr r1 r2 rr)  = do
  b1 <- getReg r1
  b2 <- getReg r2
  let b1' = if b1 > 0 then 1 else 0
  let b2' = if b2 > 0 then 1 else 0
  if b1'+b2' == 1
    then storeReg 1 rr
    else storeReg 0 rr
executeInst (Eql r1 r2 rr) = do
  b1 <- getReg r1
  b2 <- getReg r2
  if b1 == b2
    then storeReg 1 rr
    else storeReg 0 rr
executeInst (Lt r1 r2 rr)  = do
  b1 <- getReg r1
  b2 <- getReg r2
  if b1 < b2
    then storeReg 1 rr
    else storeReg 0 rr
executeInst (Not r)        = do
  res <- getReg r
  if res > 1 then storeReg 0 r else storeReg 1 r
executeInst (Store rs rr)  = do
  val <- getReg rs
  ind <- getReg rr
  storeMem val ind
executeInst (Load  rs rr)  = do
  val <- getReg rs
  storeReg val rr

buildLabelsMap :: [String] -> (Label->Int)
buildLabelsMap = buildLabelsMap' (const (-1)) 0

updateFn :: (Eq a) => (a -> b) -> a -> b -> a -> b
updateFn f a b = \x -> if x==a then b else f x

buildLabelsMap' :: (Label -> Int) -> Int -> [String] -> (Label -> Int)
buildLabelsMap' f n [] = f
buildLabelsMap' f n (i:is) = if (head i) == '.'
			       then buildLabelsMap' (updateFn f (tail i) n) n is
			       else buildLabelsMap' f (n+1) is

parseReg :: String -> Reg
parseReg str = if head str == 'r' then Reg (read $ tail str) else error "Couldn't parse register"

parseInst str = case pieces !! 0 of
		  "Add"   -> Add (parseReg (pieces !! 1)) (parseReg (pieces !! 2)) (parseReg (pieces !! 3))
		  "AddI"  -> AddI (read $ pieces !! 1) (parseReg $ pieces !! 2) (parseReg $ pieces !! 3)
		  "Sub"   -> Sub (parseReg (pieces !! 1)) (parseReg (pieces !! 2)) (parseReg (pieces !! 3))
		  "SubI"  -> SubI (read $ pieces !! 1) (parseReg $ pieces !! 2) (parseReg $ pieces !! 3)
		  "Div"   -> Div (parseReg (pieces !! 1)) (parseReg (pieces !! 2)) (parseReg (pieces !! 3))
		  "DivI"  -> DivI (read $ pieces !! 1) (parseReg $ pieces !! 2) (parseReg $ pieces !! 3)
		  "Mul"   -> Mul (parseReg (pieces !! 1)) (parseReg (pieces !! 2)) (parseReg (pieces !! 3))
		  "MulI"  -> MulI (read $ pieces !! 1) (parseReg $ pieces !! 2) (parseReg $ pieces !! 3)
		  "Mov"   -> Mov (parseReg $ pieces !! 1) (parseReg $ pieces !! 2)
		  "MovI"  -> MovI (read $ pieces !! 1) (parseReg $ pieces !! 2)
		  "Jmp"   -> Jmp (pieces !! 1)
		  "JmpR"  -> JmpR (parseReg $ pieces !! 1)
		  "JCon"  -> JCon (parseReg $ pieces !!1) (pieces !! 2) (pieces !! 3)
                  "Pc"    -> Pc (parseReg $ pieces !! 1)
		  "Print" -> Print (parseReg $ pieces !! 1)
		  "And"   -> And (parseReg (pieces !! 1)) (parseReg (pieces !! 2)) (parseReg (pieces !! 3))
		  "Or"   -> Or (parseReg (pieces !! 1)) (parseReg (pieces !! 2)) (parseReg (pieces !! 3))
		  "XOr"   -> XOr (parseReg (pieces !! 1)) (parseReg (pieces !! 2)) (parseReg (pieces !! 3))
		  "Eql" -> Eql (parseReg (pieces !! 1)) (parseReg (pieces !! 2)) (parseReg (pieces !! 3))
		  "Lt"  -> Lt (parseReg (pieces !! 1)) (parseReg (pieces !! 2)) (parseReg (pieces !! 3))
		  "Not" -> Not (parseReg $ pieces !! 1)
		  "Store" -> Store (parseReg $ pieces !! 1) (parseReg $ pieces !! 2)
		  "Load"  -> Load (parseReg $ pieces !! 1) (parseReg $ pieces !! 2)
 where pieces = words str

parseInstructions :: String -> ([Inst],Label->Int)
parseInstructions prog = (parsedInsts,labelMap)
 where instsAndLabels = filter (/="") $ lines prog
       labelMap = buildLabelsMap instsAndLabels
       parsedInsts = map parseInst $ Prelude.filter (\x -> (head x) /= '.') instsAndLabels

incCounter :: ASM ()
incCounter = modify $ \(ASMState pc is rs ms ls) -> ASMState (pc+1) is rs ms ls

evalProgram :: ASM ()
evalProgram = do
  ASMState pc is _ _ _ <- get
  if pc >= length is
    then return ()
    else do
      executeInst (is !! pc)
      pc' <- gets programCounter
      if pc' /= pc
	then evalProgram
	else incCounter >> evalProgram

printState :: ASM ()
printState = do
  ASMState pc is rs ms _ <- get
  liftIO $ do
    putStrLn $ "On instruction: " ++ (show pc)
    putStrLn $ "Registers: " ++ (show rs)

evalProgStep :: ASM ()
evalProgStep = do
  ASMState pc is _ _ _ <- get
  if pc >= length is
    then liftIO $ putStrLn "Program has finished"
    else do
      executeInst (is !! pc)
      printState
      pc' <- gets programCounter
      k <- liftIO getLine
      case k of
        "q" -> liftIO $ putStrLn "Exiting!"
        _   -> if pc' /= pc
                  then evalProgStep
                  else incCounter >> evalProgStep

initState :: [Inst] -> (Label -> Int) -> ASMState
initState code labs = ASMState 0 code (V.replicate 16 0) (V.replicate 100 0) labs

runProgram :: String -> IO ()
runProgram f = do
  insts <- readFile f
  let (code,labs) = parseInstructions insts
  evalStateT evalProgram (initState code labs)

runProgramStep :: String -> IO ()
runProgramStep f = do
  insts <- readFile f
  let (code,labs) = parseInstructions insts
  evalStateT evalProgStep (initState code labs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Need to provide a file name"
    [f] -> runProgram f
    ["--step",f] -> runProgramStep f
    otherwise -> putStrLn "Wrong number of arguments"
