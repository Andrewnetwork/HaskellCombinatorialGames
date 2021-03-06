import Test.Hspec
import TicTacToe
import Analysis
import AI

main :: IO ()
main = hspec $ do
               tests1
               tests2
               tests3
               tests4

tests1 :: Spec
tests1 = describe "otherPlayer" $ do
  it "it flips X to O " $ do
    otherPlayer X `shouldBe` O
  it "it flips O to X" $ do
    otherPlayer O `shouldBe` X

tests2 :: Spec
tests2 = describe "incGrabber" $ do
  it "it gets left to right diagonals of nonempty matrix" $ do
    incGrabber [[0,1,2],[3,4,5],[6,7,8]] 0                       `shouldBe` [0,4,8::Int]
    incGrabber [[0,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]] 0 `shouldBe` [0,5,10,15::Int]
  it "it gets left to right diagonals of nonempty board" $ do
    incGrabber [[P X, P O, P X], [P X, E , P O], [P X, E, P O]] 0 `shouldBe` [P X, E, P O]

tests3 :: Spec
tests3 = describe "isValidBoard" $ do
    it "it validates a board" $ do
        isValidBoard [[(P X),(P O),(P O)],[(P O),(P X),(P O)],[(P O),(P O),(P X)]] `shouldBe` False
        isValidBoard [[(P X),(P O),(P O)],[(P O),(P X),(P O)],[E,E,(P X)]] `shouldBe` False
        isValidBoard [[(P X),(P O),(P O)],[E,(P X),E],[E,E,(P X)]] `shouldBe` True

tests4 :: Spec
tests4 = describe "boardPositionDiff" $ do
    it "it produces a list of indicies where the elements differ between two boards" $ do
        boardPositionDiff [[E,E,E],[E,E,E],[E,E,E]] [[(P X),E,E],[E,E,E],[E,E,(P X)]] `shouldBe` [(0,0),(2,2)]
        boardPositionDiff [[E,E,E],[E,E,E],[E,E,E]] [[E,E,E],[E,E,E],[E,E,(P X)]] `shouldBe` [(2,2)]
        boardPositionDiff [[E,E,E],[E,E,E],[E,E,E]] [[(P X),E,E],[E,E,E],[E,E,E]] `shouldBe` [(0,0)]



-- printBoard [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]
-- x = gameTree O initialState
-- x = gameTreeOutcomes (gameTree O initialState)
-- showPossibleEnds [[(P O),(P X),(P O)],[(P X),(P O),(P X)],[E,E,E]] (W O) O
-- showPossibleEnds [[(P O),(P X),(P O)],[(P X),(P O),(P X)],[E,E,E]] (W X) O
-- showPossibleEnds [[(P O),(P X),(P O)],[(P X),(P O),(P X)],[E,E,E]] D O
-- ((countPlayer playerThree board)-(countPlayer (otherPlayer playerThree) board))

-- speakAboutMove O initialState (0,0)
-- speakAboutMove O [[(P O),E,E],[E,E,E],[E,E,E]] (0,0)
-- speakAboutMove O [[(P O),E,E],[E,(P O),E],[E,E,(P O)]] (2,2)


-- isTerminalState [[(P O),E,E],[E,(P O),E],[E,E,(P X)]]
-- isTerminalState [[(P O),E,E],[E,(P O),E],[E,E,(P O)]]


--isPlayerLoc [[(P O),E,E],[E,(P O),E],[E,E,(P X)]] (0,0)
-- isPieceCountValid [[(P X),E,E],[E,(P X),E],[E,E,(P X)]] -> False
-- isPieceCountValid initialState
-- isPieceCountValid [[(P O),E,E],[E,(P O),E],[E,E,(P X)]]

-- countCell (P X) [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]
-- whoWonRow [[(P X),(P X),(P X)],[(P X),(P X),(P O)],[(P X),(P X),(P O)]]
-- whoWonRow [[(P X),E,E],[E,(P X),E],[E,E,(P X)]]
-- whoWonRow (transpose [[(P X),(P X),(P X)],[(P X),(P X),(P O)],[(P X),(P X),(P O)]])


--makeRandomMove player boardState  = do let moves = validMoves player boardState
                                     -- idx <- randomRIO(0,length(moves))
                                      --move player boardState (moves !! idx)

--pathsToEndState :: EndState -> GameTree -> [GameTree]
--pathsToEndState endState (Terminal X) = X == endState
--pathsToEndState endState (Node parent children) = filter (isEndStatePath endState) children
-- wiseMover O (gameTree O [[(P X),(P O),(P X)],[E,(P O),E],[(P X),E,E]])
-- possibilityMatrix (gameTree O [[(P X),(P O),(P X)],[E,(P O),E],[(P X),E,E]])
-- = map (\countFn -> map (\(a,tree)->a) countFn gameTreeObj  childrenCounters
-- gameTree O initialState
--combinationRule (allEndStateGlobalCount O [[(P X),(P O),(P X)],[E,(P O),E],[(P X),E,E]])
-- Given a game tree, check for a path containing no end state at depth d.


-- gameTree X [[(P X),(P O),(P O)],[(P O),(P X),E],[E,E,E]]
-- countChildEndStates (W X) (gameTree X [[(P X),(P O),(P O)],[(P O),(P X),E],[E,E,E]])
-- countChildrenEndStates (W X) (gameTree X [[(P X),(P O),(P O)],[(P O),(P X),E],[E,E,E]])
--[2,2,2,1] (W X)
--[4,2,2,0] D
--[0,2,2,0] (W O)
-- (W O) - (D + (W X))

-- countChildrenEndStates (W X) (gameTree X [[(P X),(P O),(P O)],[(P O),E,(P X)],[E,E,E]])
-- [2,0,0,2] (W X)
-- [4,4,4,0] D
-- [0,2,2,4] (W O)
-- countChildrenEndStates (W O) [[(P X),(P O),(P X)],[E,(P O),E],[(P X),E,E]]

-- countChildrenEndStates (W X) (gameTree X initialState)
-- Problem: Given board a and board b, determine where the difference is.
-- [[E,E,E],[E,E,E],[E,E,E]] [[(P X),E,E],[E,E,E],[E,E,E]] -> [(0,0)]
-- [[E,E,E],[E,E,E],[E,E,E]] [[E,E,E],[E,E,E],[E,E,(P X)]] -> [(2,2)]
-- [[E,E,E],[E,E,E],[E,E,E]] [[(P X),E,E],[E,E,E],[E,E,(P X)]] -> [(0,0),(2,2)]
