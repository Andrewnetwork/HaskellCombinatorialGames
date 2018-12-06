-- symmetry.hs
-- December 2018
-- Andrew Ribeiro 

module Symmetry where
import TicTacToe 
import Data.List (transpose,elemIndex,nub,intercalate)
import Data.List.Split

data Symmetry a = Symmetry [[a]] [Bool] 
data SymmetryType = DS | CS | HS | VS | I deriving Show
data SymTree a = Empty | SymTree SymmetryType (Matrix a) [SymTree a] 
type Matrix a = [[a]]
type SymmetryPath = [SymmetryType]

instance (Show a) => (Show (Symmetry a)) where 
    show (Symmetry ls conds)
        | any (True==) conds = boardStr ++ (strSymConds conds) ++ "\n"
        | otherwise = boardStr ++ "No symmetry.\n"
        where boardStr = "\n"++concatMap (\x->x++"\n") (map show ls)

instance (Show a) => (Show (SymTree a)) where 
    show Empty = show "Empty"
    show (SymTree symType parent children) = (show symType)++(show parent)++(show children)

formatMatrix :: (Show a) => [[a]] -> String
formatMatrix []   = "┌─┐\n└─┘\n"
formatMatrix [[]] = "┌─┐\n└─┘\n"
formatMatrix xxs  = (++ bot) $ concat $ zipWith (\a b -> unlines [a, b]) (top : replicate rowC mid) rows
    where
    rowC   = pred . length $ xxs
    colC   = pred . length . head $ xxs
    top    = "┌" ++ repC "─┬" ++ "─┐"
    mid    = "├" ++ repC "─┼" ++ "─┤"
    bot    = "└" ++ repC "─┴" ++ "─┘"
    repC  = concat . replicate colC
    rows   = (++ "|") . ('|' :) . intercalate "|" . ((\x -> show x) <$>) <$> xxs

prettyPrintMatrix :: (Show a) => [[a]] -> IO ()
prettyPrintMatrix  = putStrLn . formatMatrix

getSymFlags (Symmetry _ flags) = flags 

strSymConds conds = concatMap (\x -> snd x) (filter (\x -> fst x) (zip conds ["D","C","V","H"]))


printMatrix x = "\n"++concatMap (\x->x++"\n") (map show x)

-- strSymConds (symFlags [[E,(P X),E],[(P X),E,(P X)],[(P X),E,(P X)]])
-- ('┌', '─', '┬', '┐'), ('├', '─', '┼', '┤'); , └', '─', '┴', '┘')│';
-- Symmetry [[E,(P X),E],[(P X),E,(P X)],[(P X),E,(P X)]] (symmetries [[E,(P X),E],[(P X),E,(P X)],[(P X),E,(P X)]])
-- Symmetry (chunksOf 3 [0,1,0,1,0,1,0,1,0]) (symmetries $ chunksOf 3 [0,1,0,1,0,1,0,1,0])
vert :: [[a]] -> [[a]]
vert board = map reverse board
-- vert [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]
-- map (\[a,b,c] -> [c,b,a]) board

horz :: [a] -> [a]
horz board =  reverse board
-- horz [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]
-- horz [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]
-- horz [[(P O),(P X),(P O)],[(P X),(P O),(P X)],[(P O),(P X),(P O)]]
-- horz [[(P X),(P O),(P X)],[(P X),(P O),(P X)],[(P O),(P X),(P O)]]

-- Transposing a board's rows reflects across the diagonal. 
-- Transposition is making the columns of the board, the rows of the board. 

diag :: [[a]] -> [[a]]
diag board = transpose board
-- diag [[(P X),(P O),(P X)],[(P X),(P O),(P X)],[(P O),(P X),(P O)]]
-- diag [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]
-- diag [[E,E,(P X)],[E,(P X),E],[(P X),E,E]]
-- diag [[E,(P X),(P O)],[E,(P X),E],[(P X),(P O),E]]

cdiag :: [[a]] -> [[a]]
cdiag board = diag.vert.horz $ board 
-- cdiag [[(P X),(P O),(P X)],[(P X),(P O),(P X)],[(P O),(P X),(P O)]]

-- Challenge: get the following board pointing North, use the 
-- operations defined here to make the board point East, West, South
-- x = [[E,(P X),E],[(P X),E,(P X)],[(P X),E,(P X)]]
-- West ->  diag x
-- South -> horz x
-- North -> vert x
-- East ->  cdiag x

makeGrid rows cols = chunksOf cols [1..rows*cols]

-- Given a matrix M, determine if it is symmetric with respect to cdiag,diag,horz,vert.

symFlags :: Eq a => [[a]] -> [Bool]
symFlags m = map (\x -> (x m)==m) [diag,cdiag,vert,horz]
-- symFlags [[E,(P X),E],[(P X),E,(P X)],[(P X),E,(P X)]]
-- symFlags $ chunksOf 3 [0,1,0,1,0,1,0,1,0]

makeSym mat = Symmetry mat (symFlags mat)

-- ✓ Problem: Given two matricies a,b determine what symmetric operations can be 
--             perfomed on a in order to produce b. 
symPath :: Eq a => Matrix a -> Matrix a -> [SymmetryPath]
symPath a b = map (\(path,mat)->path) (filter (\(path,mat)->mat==b) symPaths)
              where symPaths = getSymPaths [] (symTree I a [])

-- symPath [[1,2,3],[4,5,6],[7,8,9]] [[7,4,1],[8,5,2],[9,6,3]] 
-- symPath [[1,0],[1,1]] [[3,3],[3,3]] 

symPathToFunc :: SymmetryPath -> (Matrix a -> Matrix a)
symPathToFunc [] = id
symPathToFunc (x:xs) = case x of 
                        DS -> diag.(symPathToFunc xs)
                        CS -> cdiag.(symPathToFunc xs)
                        HS -> horz.(symPathToFunc xs)
                        VS -> vert.(symPathToFunc xs)
                        I  -> id.(symPathToFunc xs)
-- symPathToFunc [VS] [[0,0,1],[0,1,0],[1,0,1]]

verifyPaths :: Eq a => Matrix a -> Matrix a -> [SymmetryPath] -> Bool
verifyPaths a b paths = any (\path -> (path a) == b) (map symPathToFunc paths) 
-- verifyPaths [[1,2,3],[4,5,6],[7,8,9]] [[7,4,1],[8,5,2],[9,6,3]] (symPath [[1,2,3],[4,5,6],[7,8,9]] [[7,4,1],[8,5,2],[9,6,3]])

-- [[0,0,1],[0,1,0],[1,0,1]]
symTree :: Eq a => SymmetryType -> [[a]] -> [[[a]]] -> SymTree a
symTree symType mat hist
    | elem mat hist = Empty
    | otherwise = SymTree symType mat [diagChild,cdiagChild,horzChild,vertChild]
    where diagChild = symTree DS (diag mat) (mat:hist)
          cdiagChild = symTree CS (cdiag mat) (mat:hist)
          horzChild = symTree HS (horz mat) (mat:hist)
          vertChild = symTree VS (vert mat) (mat:hist)

lenSymTree :: Num p => SymTree a -> p
lenSymTree Empty = 0
lenSymTree (SymTree _ _ children) = foldl (+) 1 (map lenSymTree children)

getBoards :: SymTree a -> [[[a]]]
getBoards Empty = []
getBoards (SymTree _ board children) = board:(concatMap getBoards children)

getSymPaths :: SymmetryPath -> SymTree a -> [(SymmetryPath,Matrix a)]
getSymPaths symPath Empty  = []
getSymPaths symPath (SymTree sym board children) = (sym:symPath,board):(concatMap (getSymPaths (sym:symPath)) children)


-- getSymPaths [] (symTree I [[0,0,1],[0,1,0],[1,0,1]] [])
-- getSymPaths [] (symTree I [[1,2,3],[4,5,6],[7,8,9]] [])
-- [[1,2,3],[4,5,6],[7,8,9]] -> [[7,4,1],[8,5,2],[9,6,3]] | [HS,VS,CS,VS,HS,VS,I]
-- 

-- putStrLn (concatMap printMatrix (getBoards (symTree I [[0,0,1],[0,1,0],[1,0,1]] [])))
-- lenSymTree (symTree I [[1,2,3],[4,5,6],[7,8,9]] [])
-- lenSymTree (symTree I [[1,2],[3,4]] [])
-- lenSymTree (symTree I [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]] [])

-- [1,0,1],
-- [0,1,0],
-- [1,0,1]

-- [[0,1],
--  [1,0]]

--  [1,0,1],
--  [0,1,0]

-- cdiag [[0,1,0],[0,0,1],[0,0,0]]

-- [[0,0,0],
--  [0,0,1],
--  [0,1,0]]

-- symTree I [[1,2,3],[4,5,6],[7,8,9]] []
-- [[[1,2],[3,4]],[[5,6],[7,8]]]++[[9,10],[11,12]]
-- map (\x -> x OP (depth-1)) (map symTree reflections) 
-- ($mat) <$> [diag,cdiag,vert,horz]
-- children =  map (\x -> x OP (depth-1)) (map symTree reflections
-- ($ [[1,2,3],[4,5,6],[7,8,9]]) <$> [diag,cdiag,vert,horz]
--elem mat hist = Empty
-- children = map (gameTree (otherPlayer player)) boards
-- TODO: Local Symmetry 
-- strSymConds$symFlags$[[0,1,0],[0,0,1],[0,0,0]]
-- strSymConds$symFlags$[[0,1],[1,0]]
-- symTree I [[1,2],[3,4]] []
