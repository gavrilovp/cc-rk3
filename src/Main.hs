{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.Int
import qualified Data.Map as M
import Data.Map ((!), Map, insert)
import Control.Exception hiding (try)
import Control.Monad.State
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Number (int)

type Integer16 = SafeInt Int16
newtype SafeInt a = SI a
                  deriving (Show, Eq, Ord, Bounded)

type R = String

data V = Temp Integer16 | Reg R
       deriving Show

data Op2 = Move | Add | Sub
         deriving Show

data Statement = Load R
               | Store V
               | SOp2 Op2 R V
               | Loop R [Statement]
               | Cond R [Statement]
               deriving Show

type Source = [Statement]

type St = Map String Integer16

instance (Bounded a, Ord a, Num a) => Num (SafeInt a) where
  (SI a) + (SI b)
    | a > 0 && b > maxBound - a || a < 0 && b < minBound - a = error "error: overflow"
    | otherwise = SI $ a + b
  (SI a) - (SI b)
    | a > 0 && b == minBound = error "error: overflow"
    | otherwise = SI $ a + (-b)
  fromInteger = SI . fromInteger

regP :: Parser R
regP = do
  spaces
  char 'R'
  d <- digit
  return $ "R" ++ [d]

varP :: Parser V
varP =     Reg <$> regP
       <|> Temp . SI <$> int

op2P :: Parser Op2
op2P =     Move <$ string "move"
       <|> Add <$ string "add"
       <|> Sub <$ string "sub"

loadP :: Parser Statement
loadP = do
  between spaces spaces $ string "load"
  r <- regP
  return $ Load r

storeP :: Parser Statement
storeP = do
  between spaces spaces $ string "store"
  v <- varP
  return $ Store v

sop2P :: Parser Statement
sop2P = do
  spaces
  op2 <- op2P
  spaces
  r <- regP
  spaces
  v <- varP
  return $ SOp2 op2 r v

loopP :: Parser Statement
loopP = do
  spaces
  string "loop"
  spaces
  r <- regP
  sts <- statementsP
  spaces
  string "pool"
  return $ Loop r sts

condP :: Parser Statement
condP = do
  r <- between (string "cond") (string "dnoc") regP
  sts <- statementsP
  return $ Cond r sts

statementsP :: Parser [Statement]
statementsP = do
  spaces
  sts <- many1 $     try loadP
                <|> try storeP
                <|> try sop2P
                <|> try loopP
                <|> condP
  return sts

incCycles :: Integer16 -> St -> St
incCycles c st = insert "Cycles" ((st ! "Cycles") + c) st

-- asume that cond and loop can't be first statement
exec :: Statement -> St -> St
exec (Load r) st = insert "M" (st ! r) $ incCycles 1 st
exec (Store (Temp t)) st = insert "M" t $ incCycles 1 st
exec (Store (Reg r)) st = insert "M" (st ! r) $ incCycles 1 st
exec (SOp2 op2 r (Temp t)) st =
  case op2 of
   Move -> insert r t st'
   Add -> insert r (st' ! r + t) st'
   Sub -> insert r (st' ! r - t) st'
  where
    st' = incCycles 1 st
exec (SOp2 op2 r (Reg r')) st =
  case op2 of
   Move -> insert r (st' ! r') st'
   Add -> insert r (st' ! r + st' ! r') st'
   Sub -> insert r (st' ! r - st' ! r') st'
  where
    st' = incCycles 1 st
exec (Loop r sts) st =
  if st ! r > 0
  then
    exec (Loop r sts) $ foldl (flip exec) st' sts
  else
    st''
  where
    st' = incCycles 6 st
    st'' = incCycles 3 st
exec (Cond r sts) st =
  if st ! r > 0
  then
    foldl (flip exec) st' sts
  else
    st'
  where
    st' = incCycles 3 st

ls = [
  "move R1 0",
  "move R2 10",
  "loop R2",
  "  add R1 R2",
  "  sub R2 1",
  "pool",
  "store R1"
  ]

main :: IO ()
main = do
  -- store previous computation
  let initialSt = M.fromList [ ("R1", 0)
                             , ("R2", 0)
                             , ("R3", 0)
                             , ("R3", 0)
                             , ("R4", 0)
                             , ("R5", 0)
                             , ("M", 0)
                             , ("Cycles", 2)
                             ]
      sts1 = [ SOp2 Move "R1" $ Temp 0
             , SOp2 Move "R2" $ Temp 10
             , Loop "R2" [SOp2 Add "R1" $ Reg "R2", SOp2 Sub "R2" $ Temp 1]
             , Store $ Reg "R1"
             ]
      sts2 = [ SOp2 Move "R1" $ Temp 1
             , Loop "R1" [SOp2 Add "R1" $ Temp 1]
             ]
      sts3 = [SOp2 Move "R1" $ Temp 1, Cond "R1" [SOp2 Add "R1" $ Temp 2], SOp2 Add "R1" $ Temp 5]
  src <- either (fail . show) return $ parse regP "cc-rk3" "R1"
  print $ (foldl (flip exec) initialSt sts1) ! "Cycles"
  handle (\(ErrorCall e) -> print e) $ print $ foldl (flip exec) initialSt sts2
  print $ (foldl (flip exec) initialSt sts3) ! "Cycles"
  let sts = case parse statementsP "cc-rk3" (unwords ls) of
             Left err -> undefined
             Right res -> res
  print $ (foldl (flip exec) initialSt sts) ! "Cycles"
