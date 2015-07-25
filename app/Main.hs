{-# LANGUAGE TupleSections #-}
module Main (main) where

import           Control.Applicative ((<$>),(<*>))
import           Control.Monad.State
import           Data.List ((\\), intersperse)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe)

data Digit = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Eq,Ord,Enum,Bounded)
instance Show Digit where
  show x = show $ fromEnum x

-- Small coordinates
data SRow = S1 | S2 | S3 deriving (Show, Enum, Ord, Eq, Bounded)
data SCol = SA | SB | SC deriving (Show, Enum, Ord, Eq, Bounded)

-- Large coordinates
data LRow = L1 | L2 | L3 deriving (Show, Enum, Ord, Eq, Bounded)
data LCol = LA | LB | LC deriving (Show, Enum, Ord, Eq, Bounded)

type SmallField = M.Map (SRow, SCol) Digit

type BigField = M.Map (LRow, LCol) SmallField

data Constraint = Row SmallField | Col SmallField

showMbD :: Maybe Digit -> String
showMbD Nothing = "."
showMbD (Just d) = show $ succ $ fromEnum d

allVals :: (Enum a, Bounded a) => [a]
allVals = [minBound .. maxBound]

showSmall :: SmallField -> String
showSmall m = unlines $ map (\r -> concatMap (\c -> showMbD $ M.lookup (r,c) m) allVals) allVals

showBig :: BigField -> String
showBig big = let threeDigits sf sr = concat [showMbD (M.lookup (sr,sc) sf) | sc <- allVals]
                  line bf br sr = concat $ intersperse "|" [threeDigits (fromMaybe emptyField (M.lookup (br,bc) bf)) sr | bc <- allVals]
                  threeLines bf br = unlines [line bf br sr | sr <- allVals]
              in concat $ intersperse "---+---+---\n" [threeLines big br | br <- allVals]

emptyField = M.empty

select :: [a] -> [(a,[a])]
select [] = []
select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]

smallPos :: [(SRow,SCol)]
smallPos = [(x,y) | x <- allVals, y <- allVals]

getRowsD :: SRow -> SmallField -> [Digit]
getRowsD r sf = catMaybes $ map (\c -> M.lookup (r,c) sf) allVals

getColsD :: SCol -> SmallField -> [Digit]
getColsD c sf = catMaybes $ map (\r -> M.lookup (r,c) sf) allVals

guardSmallField :: Digit -> (SRow, SCol) -> Constraint -> Bool
guardSmallField d (r,_) (Row fld) = not $ elem d $ getRowsD r fld
guardSmallField d (_,c) (Col fld) = not $ elem d $ getColsD c fld

genWithConstraint :: SmallField -> [Constraint] -> [SmallField]
genWithConstraint init cs = flip evalStateT (allVals \\ M.elems init) $ (M.union init . M.unions) <$> forM (smallPos \\ M.keys init) (
    \c -> do d <- StateT select
             guard $ and $ map (guardSmallField d c) cs
             return $ M.singleton c d
    )

mkConstraint :: BigField -> (LRow, LCol) -> [Constraint]
mkConstraint bf (r,c) = let co = catMaybes [Col <$> M.lookup (t,c) bf | t <- allVals \\ [r]]
                            ro = catMaybes [Row <$> M.lookup (r,t) bf | t <- allVals \\ [c]]
                        in co ++ ro

updBig :: BigField -> (LRow, LCol) -> [BigField]
updBig init x = let cs  = mkConstraint init x
                    fld = fromMaybe emptyField $ M.lookup x init
                    fs = genWithConstraint fld cs
                in map (\a -> M.union (M.singleton x a) init) fs

solve :: BigField -> [BigField]
solve init = foldM updBig init $ [(r,c) | r <- allVals, c <- allVals]

readMbD :: Char -> Maybe Digit
readMbD '1' = Just D1
readMbD '2' = Just D2
readMbD '3' = Just D3
readMbD '4' = Just D4
readMbD '5' = Just D5
readMbD '6' = Just D6
readMbD '7' = Just D7
readMbD '8' = Just D8
readMbD '9' = Just D9
readMbD  _  = Nothing

readBig :: String -> BigField
readBig s = let mbDigs = map readMbD $ concat $ lines s
                coords = [ ((rb,cb),(rm,cm)) | rb <- allVals, rm <- allVals, cb <- allVals, cm <- allVals]
                emptyBig = M.unions [M.singleton (r,c) emptyField | r <- allVals, c <- allVals ]
                listDig = catMaybes $ zipWith (\d (xb,xm) -> ((xb,) . M.singleton xm) <$> d) mbDigs coords
            in foldl (\b (xb,sf) -> M.adjust (M.union sf) xb b) emptyBig listDig

main :: IO ()
main = do putStrLn "Need solve this:\n"
          putStrLn bigSample
          putStrLn "Solving ...\n"
          putStrLn $ showBig $ head $ solve $ readBig bigSample

bigSample = unlines ["..96.7431"
                    ,"8...53..9"
                    ,".6.2..5.."
                    ,"..89....6"
                    ,"..2.4.7.5"
                    ,".....1..."
                    ,"...5943.2"
                    ,".27.3..1."
                    ,"4..1.265."]