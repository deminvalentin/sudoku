module Main where

import qualified Data.Map as M

-- | Small (Row, Column) coordinate
data SRow = SA | SB | SC deriving (Show)
data SCol = S1 | S2 | S3 deriving (Show)

-- | Large (Row, Column) coordinate
data LRow = LA | LB | LC deriving (Show)
data LCol = L1 | L2 | L3 deriving (Show)

-- | Digits
data Dig = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 |D9 deriving (Show)

type SmallField = M.Map (SRow, SCol) Dig
type LargeField = M.Map (LRow, LCol) SmallField

main :: IO ()
main = putStrLn "Not yet working..."
