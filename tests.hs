
------------------------------------------------------------------------------------------
--                                           Tests
------------------------------------------------------------------------------------------
--
-- Rudimentary tests used for development.  
--
-- TBD   Tests should generate actions that print the expression the expression to
--       be evaluated, the value it should evaluate to and whether it passed or failed.
--
-- [ ]   Add a primitive to square to check if two board positions are the same.
--       Needed to automate unit tests.
--
-- 
------------------------------------------------------------------------------------------

module Main

where

import Data.Char
import System.IO
import Square


{-
deftest name e1 e2
  | e1 == e2 = putStrLn (name ++ ": passed")
  | otherwise = putStrLn ("name" ++ ": FAILED")

runtests = do
      test "1" (brd0 == brd0) True
      test "2" (manAt (mvMan brd0 (e2,e4)) e4) wP
      test "3" (manAt (mvMan brd0 (e2,e4)) e2) eS
-}


ruy = mvsMan brd0 [(e2,e4),(e7,e5),(g1,f3),(b8,c6),(f1,b5),(a7,a6)]
ruy2 = mvsMan ruy [(b5,a4),(g8,f6),(d2,d4),(d7,d6)]
ruy3 = mvsMan ruy2 [(a4,c6)]

brdRK = [(wK,e1),(wR,d1)]

------------------------------------------------------------------------------------------
-- Would like to simplify the crude test implementation below to something that manages
-- individaul test more cleanly along the lines of what I have above.  But I like the
-- infomation it prints and it help me with the key infomation I need to develop
-- incrementally.  E.g. it prints a readable display of the board and relevant values.
-- I want any test scaffolding I put around my tests to be as useful.
------------------------------------------------------------------------------------------


main = do
     test1
     test2
     test3
     test4
     test5
     test6
     test7
     test8
     test9
     test10
     test11
     test12
     test13
     test14
     test15
     test16
     test17
     test18
     test19
     test20
     test21
     test22
     test23
     test24
     test25
     test26
     test27
     test28
     test29
     test30
     test31
     putStrLn "done"
     

test1 = do
   putStrLn "test 1 -- print board positions"
   pBrd brd0
   pBrd (mvMan brd0 (e2,e4))
   pBrd (mvMan (mvMan brd0 (e2,e4)) (b8,c6))
   pBrd (mvMan (mvMan (mvMan brd0 (e2,e4)) (b8,c6)) (f1,b5))

test1a = do
   putStrLn "test 1a -- test if two board positions are the same"
   putStrLn "TBD"

test2 = do
   putStrLn "test 2"
   pBrd (mvsMan brd0 [(e2,e4),(b8,c6),(f1,b5)])

test3 = do
   putStrLn "test 3"
   print (pseudo brd0 b1)

test4 = do
   putStrLn "test 4"
   print (map pMove (pseudo brd0 b1))

test5= do
   putStrLn "test 5"
   pBrd [(wB,c1)]
   print (map pMove (pseudo [(wB,c1)] c1))

test6 = do
   putStrLn "test 6"
   pBrd [(wB,f4)]
   print (map pMove (pseudo [(wB,f4)] f4))

test7= do
   putStrLn "test 7"
   pBrd [(wB,f4),(wP,h2),(bB,d6)]
   print (map pMove (pseudo [(wB,f4),(wP,h2),(bB,d6)] f4))

test8 = do
   putStrLn "test 8"
   pBrd brd0
   print (map pMove (pseudo brd0 c1))

test9 = do
   putStrLn "test 9"
   pBrd ruy
   print (map pMove (pseudo ruy b5))
   print (map pMove (pseudo ruy f3))
   print (map pMove (pseudo ruy h1))
   print (map pMove (pseudo ruy d1))
   print (map pMove (pseudo ruy c1))
   print (map pMove (pseudo ruy b1))
   print (map pMove (pseudo ruy a1))

test10 = do
   putStrLn "test 10"
   print (map psq (manOn ruy White))

test11 = do
   putStrLn "test 11"
   print (map psq (manOn ruy Black))
   
test12 = do   
   putStrLn "test 12 - pawns"
   print (map pMove (pseudo ruy d2))
   print (map pMove (pseudo ruy e4))
   print (map pMove (pseudo ruy f2))

test13 = do
   putStrLn "test 13 - pawn capture"
   pBrd ruy2
   print (map pMove (pseudo ruy2 d4))
   print (map pMove (pseudo ruy2 e5))

test14 = do
   putStrLn "test 14 - king moves"
   print (map pMove (pseudo ruy2 e1))
   print (map pMove (pseudo ruy2 e8))

test15 = do
   putStrLn "test 15 - white moves"
   pBrd ruy2
   print (map pMove (foldl (\acc x -> acc ++ x) [] (map (pseudo ruy2) (manOn ruy2 White))))

test16 = do
   putStrLn "test 16 - black moves"
   print (map pMove (foldl (\acc x -> acc ++ x) [] (map (pseudo ruy2) (manOn ruy2 Black))))

test17 = do
   putStrLn "test 17 - attack"
   pBrd ruy3
   print (attacks ruy3 e8 White)
   print (attacks ruy3 e8 Black)
   print (attacks ruy3 a1 White)
   print (attacks ruy3 a1 Black)

test18 = do
   putStrLn "test 18 - black attack squares"
   pBrd brd0
   putStr "Black attacks e4 = "; print (attacks brd0 e4 Black)
   putStr "White attacks e4 = "; print (attacks brd0 e4 White)
   putStr "White attacks e3 = "; print (attacks brd0 e3 White)

test19 = do
   putStrLn "test 19 - black attack squares"
   pBrd ruy3
   putStr "White attacks e8 = "; print (attacks ruy3 e8 White)
   putStr "White attacks d8 = "; print (attacks ruy3 d8 White)
   putStr "White attacks a8 = "; print (attacks ruy3 a8 White)
   putStr "White attacks e5 = "; print (attacks ruy3 e5 White)

test20 = do
   putStrLn "test 20 - check"
   putStr "White in check ="; print (inCheck ruy3 White)
   putStr "Black in check ="; print (inCheck ruy3 Black)

test21 = do
       test21a
       test21b

test21a = do
   putStrLn "test 21a - King trials"
   pBrd [(wK,e1),(bK,e8)]
   putStr "Trial moves by White = "; print (map pMove (trials [(wK,e1),(bK,e8)] White))
   putStr "Trial moves by Black = "; print (map pMove (trials [(wK,e1),(bK,e8)] Black))

test21b = do
   putStrLn "test 21b - King + Queen trials"
   pBrd brdRK
   putStr "Trial moves by White = "; print (map pMove (trials brdRK White))
   pBrd [(wK,e1),(bK,e8),(wQ,d1),(bQ,d8)]
   putStr "Trial moves by White = "; print (map pMove (trials [(wK,e1),(bK,e8),(wQ,d1),(bQ,d8)] White))
   putStr "Trial moves by Black = "; print (map pMove (trials [(wK,e1),(bK,e8),(wQ,d1),(bQ,d8)] Black))      
   pBrd [(wK,e1),(bK,e8)]   
   pBrd brd0
   putStr "Trial moves by White = "; print (map pMove (trials brd0 White))

test22 = do
   putStrLn "test 22 - Black in Check"
   pBrd ruy3
   putStrLn "Trial moves by Black = "; print (map pMove (trials ruy3 Black))
   putStr "Black in check = "; print (inCheck ruy3 Black)
   putStr "White in check = "; print (inCheck ruy3 White)   
   putStr "Legal moves by Black = "; print (map pMove (legals ruy3 Black))

test23 = do
       test23a
       test23b

test23a = do
   putStrLn "test 23a - Minimax"
   pBrd brd0
   putStr "minimax 0 White brd0 = "; print (minimax 0 White brd0)
   putStr "minimax 1 White brd0 = "; print (minimax 1 White brd0)

test23b = do
   putStrLn "test 23b - Minimax sees capture"   
   let brd23 = [(wK,e1),(bK,e8),(bQ,e2)]
   pBrd brd23
   putStr "minimax 0 White brd23 = "; print (minimax 0 White brd23)
   putStr "minimax 1 White brd23 = "; print (minimax 1 White brd23)
   putStr "minimax 2 White brd23 = "; print (minimax 2 White brd23)
   putStr "minimax 3 White brd23 = "; print (minimax 3 White brd23)   
   putStr "minimax 0 Black brd23 = "; print (minimax 0 Black brd23)
   putStr "minimax 1 Black brd23 = "; print (minimax 1 Black brd23)
   putStr "minimax 2 Black brd23 = "; print (minimax 2 Black brd23)
   putStr "minimax 3 Black brd23 = "; print (minimax 3 Black brd23)

test24 = do
   putStrLn "test 24 - Scored Moves"
   let brd24 = [(wK,e1),(bK,e8),(bQ,e2)]
   pBrd brd24
   putStr "scoreMoves brd24 White = "; print (map psmv (scoreMoves brd24 White))

test25 = do
   putStrLn "test 25 - Scored Moves"
   let brd25 = [(wK,e1),(bK,e8),(bQ,a2)]
   pBrd brd25
   putStr "scoreMoves brd25 White = "; print (map psmv (scoreMoves brd25 White))

test26 = do
   putStrLn "test 26 - Scored Moves"
   let brd26 = [(wK,e1),(bK,d3),(bQ,a2)]
   pBrd brd26
   putStr "scoreMoves brd26 White = "; print (map psmv (scoreMoves brd26 White))

test27 = do
   putStrLn "test 27 - Scored Moves"
   let brd27 = [(wK,e1),(bK,d3),(bQ,a2),(wN,c3)]
   pBrd brd27
   putStr "scoreMoves brd27 White = "; print (map psmv (scoreMoves brd27 White))
   putStr "bestScoreMove brd27 White = "; print (psmv (bestScoreMove brd27 White))
   putStr "scoreMoves brd27 Black = "; print (map psmv (scoreMoves brd27 Black))   
   putStr "bestScoreMove brd27 Black = "; print (psmv (bestScoreMove brd27 Black))

test28 = do
   putStrLn "test 28 - Alphabeta"
   pBrd brd0
   putStr "alphabeta brd0 0 (minBound::Int) (maxBound::Int) White = "; print (alphabeta brd0 0 (minBound::Int) (maxBound::Int) White)
   putStr "alphabeta brd0 1 (minBound::Int) (maxBound::Int) White = "; print (alphabeta brd0 1 (minBound::Int) (maxBound::Int) White)
   putStr "alphabeta brd0 2 (minBound::Int) (maxBound::Int) White = "; print (alphabeta brd0 2 (minBound::Int) (maxBound::Int) White)

test29 = do
   putStrLn "test 29 - Alphabeta sees capture"   
   let brd29 = [(wK,e1),(bK,e8),(bQ,e2)]
   pBrd brd29
   putStr "alphabeta brd29 0 (minBound::Int) (maxBound::Int) White = "; print (alphabeta brd29 0 (minBound::Int) (maxBound::Int) White)
   putStr "alphabeta brd29 1 (minBound::Int) (maxBound::Int) White = "; print (alphabeta brd29 1 (minBound::Int) (maxBound::Int) White)
   putStr "alphabeta brd29 2 (minBound::Int) (maxBound::Int) White = "; print (alphabeta brd29 2 (minBound::Int) (maxBound::Int) White)
   putStrLn "done"

test30 = do
   putStrLn "test 30 - Alphabeta in ruy"
   pBrd ruy3
   putStr "alphabeta ruy3 0 (minBound::Int) (maxBound::Int) White = "; print (alphabeta ruy3 0 (minBound::Int) (maxBound::Int) White)
   putStr "alphabeta ruy3 1 (minBound::Int) (maxBound::Int) White = "; print (alphabeta ruy3 1 (minBound::Int) (maxBound::Int) White)
   putStr "alphabeta ruy3 2 (minBound::Int) (maxBound::Int) White = "; print (alphabeta ruy3 2 (minBound::Int) (maxBound::Int) White)
   putStr "alphabeta ruy3 3 (minBound::Int) (maxBound::Int) White = "; print (alphabeta ruy3 3 (minBound::Int) (maxBound::Int) White)
  -- putStr "alphabeta ruy3 4 (minBound::Int) (maxBound::Int) White = "; print (alphabeta ruy3 4 (minBound::Int) (maxBound::Int) White)
  -- putStr "alphabeta ruy3 5 (minBound::Int) (maxBound::Int) White = "; print (alphabeta ruy3 5 (minBound::Int) (maxBound::Int) White)
   putStr "alphabeta ruy3 0 (minBound::Int) (maxBound::Int) Black = "; print (alphabeta ruy3 0 (minBound::Int) (maxBound::Int) Black)
   putStr "alphabeta ruy3 1 (minBound::Int) (maxBound::Int) Black = "; print (alphabeta ruy3 1 (minBound::Int) (maxBound::Int) Black)
   putStr "alphabeta ruy3 2 (minBound::Int) (maxBound::Int) Black = "; print (alphabeta ruy3 2 (minBound::Int) (maxBound::Int) Black)
   putStr "alphabeta ruy3 3 (minBound::Int) (maxBound::Int) Black = "; print (alphabeta ruy3 3 (minBound::Int) (maxBound::Int) Black)
  -- putStr "alphabeta ruy3 4 (minBound::Int) (maxBound::Int) Black = "; print (alphabeta ruy3 4 (minBound::Int) (maxBound::Int) Black)
  -- putStr "alphabeta ruy3 5 (minBound::Int) (maxBound::Int) Black = "; print (alphabeta ruy3 5 (minBound::Int) (maxBound::Int) Black)

test31 = do
   putStrLn "test 31 - Mate in 2"
   let brd31 = [(wK,g1),(wP,f2),(wP,g2),(wP,h2),(bK,e8),(bR,d8),(bR,d7),(wR,d1),(wQ,a1)]
   pBrd brd31
   putStr "scoreMoves brd31 Black ="; print (map psmv (scoreMoves brd31 Black))