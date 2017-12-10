-- ---------------------------------------------------------------------------------------
--
--                                     Haskell You a Chess 
--
--                                      Mark R. Cornwell
--                                      December 1, 2017
--
-- ---------------------------------------------------------------------------------------
--    $ ghc -O square
--    $ ./square
--    bR bN bB bQ bK bB bN bR
--    bP bP bP bP bP bP bP bP
--    -- -- -- -- -- -- -- --
--    -- -- -- -- -- -- -- --
--    -- -- -- -- -- -- -- --
--    -- -- -- -- -- -- -- --
--    wP wP wP wP wP wP wP wP
--    wR wN wB wQ wK wB wN wR
--    |
--    Welcome to Haskell Chess!
--    White to move; may I suggest e2e4?
-- ---------------------------------------------------------------------------------------

module Main

where

import Data.Char
import Control.Monad
import System.IO
import Data.Tree

------------------------------------------------------------------------------------------
-- Square Geometry
--
-- We decided to implement squares using a natural mapping to the integers 0..63
--
-- Aside. Another way to go might have been to enumerate the squares as data and then
-- define all the relationships directly without appealing to the numbering.
-- But the numbering is simple and familiar. It would be interesting to do a
-- version using Haskell to define logical relationships between enumerated
-- square names without any appeal to a numbering. Curious to know if it would
-- be any clearer or more easily optimized by the compiler. (End of Aside)
------------------------------------------------------------------------------------------

type Square = Int

sqr f r = f + r * 8
rnk s = s `div` 8
fil s = s `mod` 8
isq s = -1 < s && s < 64

psq s = ['a'..'h'] !! fil s : show (rnk s + 1)
rsq t = sqr (ord (head t) - ord 'a') (ord (head (tail t)) - ord '1')

-- Numbering the squares on the chessboard

a8 = 56; b8 = 57; c8 = 58; d8 = 59; e8 = 60; f8 = 61; g8 = 62; h8 = 63;
a7 = 48; b7 = 49; c7 = 50; d7 = 51; e7 = 52; f7 = 53; g7 = 54; h7 = 55;
a6 = 40; b6 = 41; c6 = 42; d6 = 43; e6 = 44; f6 = 45; g6 = 46; h6 = 47;
a5 = 32; b5 = 33; c5 = 34; d5 = 35; e5 = 36; f5 = 37; g5 = 38; h5 = 39;
a4 = 24; b4 = 25; c4 = 26; d4 = 27; e4 = 28; f4 = 29; g4 = 30; h4 = 31;
a3 = 16; b3 = 17; c3 = 18; d3 = 19; e3 = 20; f3 = 21; g3 = 22; h3 = 23;
a2 = 08; b2 = 09; c2 = 10; d2 = 11; e2 = 12; f2 = 13; g2 = 14; h2 = 15
a1 = 00; b1 = 01; c1 = 02; d1 = 03; e1 = 04; f1 = 05; g1 = 06; h1 = 07

------------------------------------------------------------------------------------------
-- Relationships between the squares
--
-- Having enumerated a numbering for squares, we take advantage of this numbering
-- to enumberate a serives of functions that establish relationships between sequares.
-- E.g. E.g. what square is on square forwared of a given square.  E.g. fwd e2 = e3.
-- Instead of enumerating every relationship (which we could do) we leverage the
-- numbering in hope of advantages in hopes of advantagess in efficiency and simplicty.
-- It also leverages the board representation worked out in lisp, python, and C++
-- implemenations of this chess program.
------------------------------------------------------------------------------------------

fwd s = if isq s && rnk s < 7 then s + 8 else 99
bak s = if isq s && rnk s > 0 then s - 8 else 99
lft s = if isq s && fil s > 0 then s - 1 else 99
rgt s = if isq s && fil s < 7 then s + 1 else 99

dfr = fwd . rgt
dfl = fwd . lft
dbr = bak . rgt
dbl = bak . lft

------------------------------------------------------------------------------------------
-- Move Geometry
--
-- The routines below describe what suqares a Man could move to if placed on an empty
-- chessboard.
--
-- Not everything below is used, but it is informative to see how we could
-- calcualte moves on an empty chessboard.  For move generation, the functions below
-- had to be modified to account for the presence of other pieces on the chessboard.  
------------------------------------------------------------------------------------------

mvN :: Int -> [Int]
mvN s = [x | x <- dfr(fwd s):dfl(fwd s):dfr(rgt s):dbr(rgt s):dbr(bak s):dbl(bak s):dbl(lft s):dfl(lft s):[] , isq x]

mvK s = [x | x <- (fwd s):(dfr s):(rgt s):(dbr s):(bak s):(dbl s):(lft s):(dfl s):[] , isq x]

sli dir s l = if isq (dir s) then (dir s) : sli dir (dir s) l else []

mvB s = (sli dfr s []) ++ (sli dfl s []) ++ (sli dbr s []) ++ (sli dbl s [])
mvR s = (sli fwd s []) ++ (sli bak s []) ++ (sli rgt s []) ++ (sli lft s [])
mvQ s = (mvB s) ++ (mvR s)

------------------------------------------------------------------------------------------
-- Man
--
-- Representation of the chess pieces and pawns.  In chess parlayance a piece is a King,
-- Queen, Rook, Bishop, or Knight.  Technically a Pawn is not a Piece.  So I use the term
-- Men because a Pawn is considred a Man.  Again, I map the Men to a numbering for the
-- same reasons that I mapped squares to a numbering.
------------------------------------------------------------------------------------------

data Kind = King | Queen | Bishop | Knight | Rook | Pawn
     deriving (Eq, Show, Ord)

data Color = White | Black | None deriving (Eq,Show,Ord)

type Man = Int

wK = 1 :: Int; wQ = 2 :: Int; wR = 3 :: Int; wB =  4 :: Int; wN =  5 :: Int; wP =  6 :: Int
bK = 7 :: Int; bQ = 8 :: Int; bR = 9 :: Int; bB = 10 :: Int; bN = 11 :: Int; bP = 12 :: Int

eS = 99 :: Int    -- empty (i.e. unoccupied) square

kind :: Man -> Kind
kind m = [King,Queen,Rook,Bishop,Knight,Pawn] !! ((m - 1) `mod` 6)

color :: Man -> Color
color 99 = None
color m = if m < bK then White else Black

------------------------------------------------------------------------------------------
-- Actions for printing men based on our numbering.  Essentially we map the men into their
-- representation space as numbers and map those numbers to strings we can use for printing
-- the men when we need to print out the chessboard to the console in a human readable form.
------------------------------------------------------------------------------------------

pman :: Man -> String
pman 1 = "wK"; pman 2 = "wQ"; pman 3 = "wR"; pman  4 = "wB"; pman  5 = "wN"; pman  6 = "wP"
pman 7 = "bK"; pman 8 = "bQ"; pman 9 = "bR"; pman 10 = "bB"; pman 11 = "bN"; pman 12 = "bP"
pman 99 = "--"
pman x = "??"



------------------------------------------------------------------------------------------
-- Board
--
-- We chose to represent the Board simply as a list of pairs indicating a Man and what
-- square they are on.  This may need to be expanded later with such things as the
-- existance of en-passant squares, flags indicating castling options.  A challenge to
-- to this cleanly without cluttering the logic of the simple cases.
------------------------------------------------------------------------------------------

type Brd = [(Man,Square)]

brd0 = [(bR,a8),(bN,b8),(bB,c8),(bQ,d8),(bK,e8),(bB,f8),(bN,g8),(bR,h8)
       ,(bP,a7),(bP,b7),(bP,c7),(bP,d7),(bP,e7),(bP,f7),(bP,g7),(bP,h7)
       ,(wP,a2),(wP,b2),(wP,c2),(wP,d2),(wP,e2),(wP,f2),(wP,g2),(wP,h2)
       ,(wR,a1),(wN,b1),(wB,c1),(wQ,d1),(wK,e1),(wB,f1),(wN,g1),(wR,h1)
       ]

brd1 = [(wN,e4)]

manAt brd sq = let mm = [ m | (m,s) <- brd, s == sq ] in
               if mm == [] then 99 else head mm

pManAt brd sq = pman (manAt brd sq)

pBrd brd = do
   mapM_ (\r -> putStrLn (pBrd' brd r)) ranks
   putStrLn "|"
      where
        pBrd' brd lsq = foldl (\acc x -> acc ++ " " ++ x) [] (map (pManAt brd) lsq)
        ranks = [[a8..h8],[a7..h7],[a6..h6],[a5..h5],[a4..h4],[a3..h3],[a2..h2],[a1..h1]]

------------------------------------------------------------------------------------------
-- Moves
--
-- A move is a pair.  Given a chessboard you will pick up the piece form the
-- first square and place it down on the second square.  That is the intutition
-- behind the representation.  Of course there will be complications.  If the
-- square you move to has an opponents piece on it you will need to remove that
-- piece, or you will need to remember it if you later need to undo the move.
-- We table these issues for later.  The simplicity of a viewing a Move mainly
-- as a pair of squares quite appealing.
------------------------------------------------------------------------------------------

type Move = (Square,Square)

pMove :: Move -> String
pMove mv = psq s ++ psq s' where (s,s') = mv

rMove :: String -> Move
rMove str = (rsq (take 2 str), rsq (drop 2 str))

------------------------------------------------------------------------------------------
-- A primitive move function that ignores any rules, just picks up a man from
-- one square on the board and puts him down on another.  If there is a man
-- on the square you are moving to, that man goes away.
------------------------------------------------------------------------------------------

mvMan :: Brd -> Move -> Brd
mvMan brd mv = mvMan' brd mv brd

mvMan' brd mv [] = [(manAt brd s, s')] where (s,s') = mv
mvMan' brd mv brd'
   | sq == s   = mvMan' brd mv xs
   | sq == s'  = mvMan' brd mv xs
   | otherwise  = (m,sq):mvMan' brd mv xs
   where (m,sq):xs = brd'
         (s,s')    = mv

mvsMan :: Brd -> [Move] -> Brd
mvsMan brd mvs
    | xs == []  = mvMan brd mv
    | otherwise = mvsMan (mvMan brd mv) xs
    where mv:xs = mvs

--------------------------------------------------------------------------------------
-- Pseudo legal Moves are moves that are almost legal.  Pieces move the right squares
-- ingoring the rule about not moving into check.  Out strategy for calcualting moves
-- is to calcuate the pseudo legal moves first, then filer out those that move into
-- check.
--------------------------------------------------------------------------------------

pseudo :: Brd -> Square -> [Move]
pseudo brd sq = case k of
                   Knight   -> pseudoN brd sq
                   Bishop   -> pseudoB brd sq
                   Rook     -> pseudoR brd sq
                   Queen    -> pseudoQ brd sq
                   Pawn     -> pseudoP brd sq
                   King     -> pseudoK brd sq
                 where k = kind (manAt brd sq)

pseudoN :: Brd -> Square -> [Move]
pseudoN brd sq = filter (diffColor brd) (map (\s -> (sq,s)) (mvN sq))

pseudoK :: Brd -> Square -> [Move]
pseudoK brd sq = filter (diffColor brd) (map (\s -> (sq,s)) (mvK sq))

-- note that any unoccupied squares will have the color None
diffColor brd mv = color (manAt brd s1) /= color (manAt brd s2) where (s1,s2) = mv

pseudoB :: Brd -> Square -> [Move]
pseudoB brd sq = (f dfr) ++ (f dfl) ++ (f dbr) ++ (f dbl) where f = slide brd sq sq

pseudoR :: Brd -> Square -> [Move]
pseudoR brd sq = (f fwd) ++ (f bak) ++ (f rgt) ++ (f lft) where f = slide brd sq sq

pseudoQ :: Brd -> Square -> [Move]
pseudoQ brd sq = (pseudoB brd sq) ++ (pseudoR brd sq)

slide :: Brd -> Square -> Square -> (Square -> Square) -> [Move]
slide brd sq s dir
    | isq s' && sqEmpty brd s' = (sq,s'):slide brd sq s' dir
    | isq s' && diffColor brd (sq,s') = [(sq,s')]
    | otherwise                   = []
    where s' = dir s

sqEmpty :: Brd -> Square -> Bool
sqEmpty brd sq = (color (manAt brd sq)) == None    -- color of an empty square is None

manOn :: Brd -> Color -> [Square]
manOn brd c = [s | (m,s) <- brd, color m == c]

pseudoP :: Brd -> Square -> [Move]
pseudoP brd sq
   | color (manAt brd sq) == White  = pseudoP' brd sq (rnk a2) dfr fwd dfl
   | color (manAt brd sq) == Black  = pseudoP' brd sq (rnk a7) dbr bak dbl
   | otherwise = error "pseudoP: sq cannot be empty"

pseudoP' :: Brd -> Square -> Int -> (Square -> Square) -> (Square -> Square) -> (Square -> Square) -> [Move]
pseudoP' brd sq home dr d dl =
   (if (rnk sq) == home && sqEmpty brd (d sq) && sqEmpty brd (d (d sq)) then [(sq,(d (d sq)))] else []) ++
   (if sqEmpty brd (d sq) then [(sq,(d sq))] else []) ++
   (if enemyAt brd sq (dr sq) then [(sq,(dr sq))] else []) ++
   (if enemyAt brd sq (dl sq) then [(sq,(dl sq))] else [])

enemyAt :: Brd -> Square -> Square -> Bool
enemyAt brd sq s' = (color (manAt brd s') /= None) && (color (manAt brd s') /= color (manAt brd sq))

------------------------------------------------------------------------------------------
-- Attacks
--
-- Determines if a square is under attack by a man of a given color.  E.g. does Black
-- attack e4?  Used determine if the King is in check.  A mandatory pre-requisite for
-- filtering out illegal moves.  Was impressed how short these become in Haskell while
-- remaining readable.
-----------------------------------------------------------------------------------------

attacks :: Brd -> Square -> Color -> Bool
attacks brd sq c = (attackK brd sq c) || (attackBQ brd sq c) || (attackRQ brd sq c) || (attackP brd sq c)

attackK :: Brd -> Square -> Color -> Bool
attackK brd sq c = f fwd || f bak || f rgt || f lft || f dfr || f dfl || f dbr || f dbl where f = attackK' brd sq c

attackK' :: Brd -> Square -> Color -> (Square -> Square) -> Bool
attackK' brd sq c dir = (color m == c) && (kind m == King) where m = manAt brd (dir sq)

attackBQ :: Brd -> Square -> Color -> Bool
attackBQ brd sq c = f dfr || f dfl || f dbr || f dbl where f = attackBQ' brd sq c

attackBQ' :: Brd -> Square -> Color -> (Square -> Square) -> Bool
attackBQ' brd sq c dir = (color m == c) && ((kind m) `elem` [Bishop,Queen]) where m = manAt brd (seek brd sq dir)

attackRQ :: Brd -> Square -> Color -> Bool
attackRQ brd sq c = f fwd || f bak || f rgt || f lft where f = attackRQ' brd sq c

attackRQ' :: Brd -> Square -> Color -> (Square -> Square) -> Bool
attackRQ' brd sq c dir = (color m == c) && ((kind m) `elem` [Rook,Queen]) where m = manAt brd (seek brd sq dir)

attackP :: Brd -> Square -> Color -> Bool
attackP brd sq c 
   | c == White = (attackP' brd (dbr sq) c) || (attackP' brd (dbl sq) c)
   | c == Black = (attackP' brd (dfr sq) c) || (attackP' brd (dfl sq) c)

attackP' brd sq c = (color m == c) && (kind m == Pawn) where m = manAt brd sq

------------------------------------------------------------------------------------------
-- Seek from a given square on the board out in particular direction until you either
-- fall off the board or run into a man.  Useful to find out if a given square is under
-- attack by a Bishop, Rook, or Queen.
------------------------------------------------------------------------------------------

seek :: Brd -> Square -> (Square -> Square) -> Square
seek brd sq dir
   | dir sq == 99                  = 99                       -- off the board
   | manAt brd (dir sq) == 99      = seek brd (dir sq) dir    -- empty square
   | otherwise                     = dir sq                   -- square holds a Man

------------------------------------------------------------------------------------------
-- Legals
--
-- Calculate all the pseuodo legal moves in a position and filter out those that
-- move into check.
-------------------------------------------------------------------------------------------

opposite White = Black
opposite Black = White

kingSq :: Brd -> Color -> Square
kingSq [] c = eS
kingSq brd c
   | (kind m == King) && (color m == c)   = s
   | otherwise                            = kingSq xs c
       where (m,s):xs = brd

inCheck ::  Brd -> Color -> Bool
inCheck brd c = kingSq brd c /= 99 && attacks brd (kingSq brd c) (opposite c)

legal :: Brd -> Move -> Bool
legal brd mv = not (inCheck (mvMan brd mv) (color m)) where (m,_) = mv

trials :: Brd -> Color -> [Move]
trials brd c = trials' brd brd c

trials' :: Brd -> [(Man,Square)] -> Color -> [Move]
trials' brd [] c = []
trials' brd men c
   | color m == c   = (pseudo brd s) ++ (trials' brd xs c)
   | otherwise      = trials' brd xs c
   where (m,s):xs = men

legals :: Brd -> Color -> [Move]
legals brd c = filter (legal brd) (trials brd c)

------------------------------------------------------------------------------------------
-- Static Evaluation
--
-- We score the position on some very simple factors.  First is material advantage.
-- We can extend this later as needed. Typically programs play better by virtue of a
-- deeper move search. Cycles spent in evaluating a static position incurs a penalty
-- in how many positions we can evaluate in a given amount of time.  We can experiment
-- to optimize this tradeoff based on strenth of play.  Consider using an adaptive
-- machine learing approach to help optimize these tradeoffs.
------------------------------------------------------------------------------------------

score brd = (material brd)*10 + (development brd)

material brd = (sum (map val (map fst brd)))

-- These corrections for development give bounus for developing center pawn 2 squares
-- and penalize Bishops and Knights left on their original squares.  We ought to turn
-- this off after move 10 or so.

development [] = 0
development brd
    | ms == (wP,e4)  =  1 + development xs 
    | ms == (wP,d4)  =  1 + development xs
    | ms == (wP,c4)  =  1 + development xs
    | ms == (bP,e5)  = -1 + development xs 
    | ms == (bP,d5)  = -1 + development xs
    | ms == (bP,c5)  = -1 + development xs
    | ms == (wN,b1)  = -1 + development xs
    | ms == (wN,g1)  = -1 + development xs
    | ms == (wB,c1)  = -1 + development xs
    | ms == (wB,f1)  = -1 + development xs
    | ms == (bN,b8)  =  1 + development xs
    | ms == (bN,g8)  =  1 + development xs
    | ms == (bB,c8)  =  1 + development xs
    | ms == (bB,f8)  =  1 + development xs
    | otherwise      =  development xs
    where ms:xs = brd

val :: Man -> Int
val 1 =  999; val 2 =  9; val 3 =  5; val  4 =  3; val  5 =  3; val  6 =  1
val 7 = -999; val 8 = -9; val 9 = -5; val 10 = -3; val 11 = -3; val 12 = -1

------------------------------------------------------------------------------------------
-- Minimax search
--
-- 'minimax 3 White brd' says that from the given board position brd, if we look 3 half
--  moves deep, assuming White has the move, we score the position as the value returned.
------------------------------------------------------------------------------------------

minimax :: Int -> Color -> Brd -> Int

minimax depth side brd
    | depth == 0               = score brd
    | nextBrds brd side == []  = scoreTerminal side brd
    | side == White            = maximum (map (minimax (depth - 1) Black) (nextBrds brd White))
    | side == Black            = minimum (map (minimax (depth - 1) White) (nextBrds brd Black))


------------------------------------------------------------------------------------------
-- Alphabeta search     Ref: https://en.wikipedia.org/wiki/Alpha–beta_pruning
--
-- I manually transformed the pseudocode on the Wikipedia page into a Single Static
-- Assigment (SSA) form which works in Haskell.  Try it!
------------------------------------------------------------------------------------------

-- initial call
-- alphabeta origin depth minBound::Int maxBound::Int White
{-
alphabeta :: Brd -> Int -> Int -> Int -> Color -> Int
alphabeta brd depth alpha beta side
    | depth == 0               = score brd
    | children == []           = scoreTerminal side brd
    | side == White            = maximizingPlayer depth (minBound::Int) alpha beta children
    | side == Black            = minimizingPlayer depth (maxBound::Int) alpha beta children
    where children = nextBrds brd side

maximizingPlayer depth v alpha beta [] = v
maximizingPlayer depth v alpha beta brds =
     if beta <= alpha' then v else maximizingPlayer depth v' alpha' beta xs
     where v'       = max v (alphabeta child (depth - 1) alpha beta Black)
           alpha'   = max alpha v'
           child:xs = brds

minimizingPlayer depth v alpha beta [] = v
minimizingPlayer depth v alpha beta brds =
     if beta' <= alpha then v else minimizingPlayer depth v' alpha beta' xs
     where v'       = min v (alphabeta child (depth - 1) alpha beta White)
           beta'    = min beta v'
           child:xs = brds
-}

-- https://chessprogramming.wikispaces.com/Alpha-Beta

alphabeta :: Brd -> Int -> Int -> Int -> Color -> Int
alphabeta brd depth alpha beta side
   | side == White   = alphaBetaMax minBound maxBound depth brd
   | side == Black   = alphaBetaMin minBound maxBound depth brd

alphaBetaMax alpha beta depth brd
   | depth == 0      = score brd
   | children == []  = alpha
   | otherwise       = forAlphaBetaMax alpha beta depth children
   where children = nextBrds brd White

forAlphaBetaMax alpha beta depth [] = alpha
forAlphaBetaMax alpha beta depth brds
   | v >= beta    = beta
   | v > alpha    = forAlphaBetaMax v beta depth xs
   | otherwise    = forAlphaBetaMax alpha beta depth xs
   where child:xs = brds
         v = alphaBetaMin alpha beta (depth - 1) child

alphaBetaMin alpha beta depth brd
   | depth == 0      = score brd
   | children == []  = beta
   | otherwise       = forAlphaBetaMin alpha beta depth children
   where children = nextBrds brd Black   

forAlphaBetaMin alpha beta depth [] = beta
forAlphaBetaMin alpha beta depth brds
   | v <= alpha   = alpha
   | v < beta     = forAlphaBetaMin alpha v depth xs
   | otherwise    = forAlphaBetaMin alpha beta depth xs
   where child:xs = brds
         v = alphaBetaMax alpha beta (depth - 1) child


----------------------------------------------------------------------------------------
-- helper functions common to both search algorithms
----------------------------------------------------------------------------------------

nextBrds brd c = map (mvMan brd) (legals brd c)

-- this version of score Terminial is robust in that it should work whether we do legal moves
-- or proceed with trial moves.  Trial moves may speed up the search, king captures will be found
-- by the minimax search so by not filtering illegal moves, move generation can be faster.

scoreTerminal side brd
    | side == White  = if (kingSq brd White) == eS || inCheck brd White then -99999999 else 0
    | side == Black  = if (kingSq brd Black) == eS || inCheck brd Black then  99999999 else 0


-- Redefine side to switch between minimax and alphabeta search
value side brd = minimax maxDepth side brd

-- value side brd = alphabeta brd maxDepth (minBound::Int) (maxBound::Int) side

maxDepth = 4

scoreMoves :: Brd -> Color -> [(Int,Move)]

scoreMoves brd side = [(value (opposite side) (mvMan brd mv),mv) | mv <- legals brd side]

psmv :: (Int,Move) -> String
psmv (v,mv) = show v ++ ":" ++ pMove mv

bestScoreMove :: Brd -> Color -> (Int,Move)
bestScoreMove brd White = foldl (\x y -> (if (fst x) > (fst y) then x else y)) (minBound::Int,(a1,a1)) (scoreMoves brd White)
bestScoreMove brd Black = foldl (\x y -> (if (fst x) < (fst y) then x else y)) (maxBound::Int,(a1,a1)) (scoreMoves brd Black)

bestMove brd side = mv where (v,mv) = bestScoreMove brd side

------------------------------------------------------------------------------------------
-- User Command Loop
------------------------------------------------------------------------------------------

main = test31

into = do
      pBrd brd0     
      putStrLn "Welcome to Haskell Chess!"
      putStrLn "White to move; may I suggest e2e4?"
      command brd0 White

command brd side = do
     putStr "move? "; hFlush stdout
     cmd <- getLine
     putStrLn $ "You typed: " ++ cmd
     dispatch brd side cmd

dispatch brd side "help" = do putStrLn "[help test show value quit e1e2]"
                              command brd side

dispatch brd side "test" = do tests
                              command brd side

dispatch brd side "show" = do
     pBrd brd
     putStrLn (show side ++ " to Move")
     command brd side

dispatch brd side "" = dispatch brd side "show"

dispatch brd side "value" = do putStrLn (show (value side brd))
                               command brd side

dispatch brd side "quit" = do putStrLn "Exiting command loop..."

dispatch brd side cmd
     | mv `elem` legals brd side = do putStrLn (show side ++ " moves " ++ cmd)
                                      pBrd (mvMan brd mv)
                                      putStrLn ((show (opposite side)) ++ " moves " ++ (pMove (bestMove (mvMan brd mv) (opposite side))))
                                      pBrd (mvMan (mvMan brd mv) (bestMove (mvMan brd mv) (opposite side)))
                                      command (mvMan (mvMan brd mv) (bestMove (mvMan brd mv) (opposite side))) side
     | otherwise                 = do putStrLn (cmd ++ " is NOT a legal move")
                                      command brd side
     where mv = rMove cmd

------------------------------------------------------------------------------------------
--                                            TBD
------------------------------------------------------------------------------------------
-- [12/3] Static score   
-- [12/3] Minimax search
-- [12/3] Command Loop
-- [12/4] Alphabeta search  (In Work)
-- [  ] Pawn promotion (simplified)
-- [  ] Pawn promotion (complete)
-- [  ] Castling
-- [  ] En Passant
-- [  ] Checkmate
-- [  ] Stalemate
-- [  ] KMoves/Sec Metric
-- [  ] Think while waiting
-- [  ] Itterative deepening
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
--                                           Tests
------------------------------------------------------------------------------------------
--
-- tests should generate actions that print the expression the expression to
-- be evaluated, the value it should evaluate to and whether it passed or failed.
------------------------------------------------------------------------------------------

{-
deftest name e1 e2
  | e1 == e2 = putStrLn (name ++ ": passed")
  | otherwise = putStrLn ("name" ++ ": FAILED")

runtests = do
      test "1" (brd0 == brd0) True
      test "2" (manAt (mvMan brd0 (e2,e4)) e4) wP
      test "3" (manAt (mvMan brd0 (e2,e4)) e2) eS
-}

ruy = (mvsMan brd0 [(e2,e4),(e7,e5),(g1,f3),(b8,c6),(f1,b5),(a7,a6)])
ruy2 = (mvsMan ruy [(b5,a4),(g8,f6),(d2,d4),(d7,d6)])
ruy3 = (mvsMan ruy2 [(a4,c6)])

brdRK = [(wK,e1),(wR,d1)]

------------------------------------------------------------------------------------------
-- Would like to simplify the crude test implementation below to something that manages
-- individaul test more cleanly along the lines of what I have above.  But I like the
-- infomation it prints and it help me with the key infomation I need to develop
-- incrementally.  E.g. it prints a readable display of the board and relevant values.
-- I want any test scaffolding I put around my tests to be as useful.
------------------------------------------------------------------------------------------

tests = do
   pBrd brd0
   pBrd (mvMan brd0 (e2,e4))
   pBrd (mvMan (mvMan brd0 (e2,e4)) (b8,c6))
   pBrd (mvMan (mvMan (mvMan brd0 (e2,e4)) (b8,c6)) (f1,b5))
   
   putStrLn "test 2"
   pBrd (mvsMan brd0 [(e2,e4),(b8,c6),(f1,b5)])
   
   putStrLn "test 3"
   print (pseudo brd0 b1)
   
   putStrLn "test 4"
   print (map pMove (pseudo brd0 b1))
   
   putStrLn "test 5"
   pBrd [(wB,c1)]
   print (map pMove (pseudo [(wB,c1)] c1))
   
   putStrLn "test 6"
   pBrd [(wB,f4)]
   print (map pMove (pseudo [(wB,f4)] f4))
   
   putStrLn "test 7"
   pBrd [(wB,f4),(wP,h2),(bB,d6)]
   print (map pMove (pseudo [(wB,f4),(wP,h2),(bB,d6)] f4))
   
   putStrLn "test 8"
   pBrd brd0
   print (map pMove (pseudo brd0 c1))
   
   putStrLn "test 9"
   pBrd ruy
   print (map pMove (pseudo ruy b5))
   print (map pMove (pseudo ruy f3))
   print (map pMove (pseudo ruy h1))
   print (map pMove (pseudo ruy d1))
   print (map pMove (pseudo ruy c1))
   print (map pMove (pseudo ruy b1))
   print (map pMove (pseudo ruy a1))
   
   putStrLn "test 10"
   print (map psq (manOn ruy White))
   
   putStrLn "test 11"
   print (map psq (manOn ruy Black))
   
   putStrLn "test 12 - pawns"
   print (map pMove (pseudo ruy d2))
   print (map pMove (pseudo ruy e4))
   print (map pMove (pseudo ruy f2))
   
   putStrLn "test 13 - pawn capture"
   pBrd ruy2
   print (map pMove (pseudo ruy2 d4))
   print (map pMove (pseudo ruy2 e5))
   
   putStrLn "test 14 - king moves"
   print (map pMove (pseudo ruy2 e1))
   print (map pMove (pseudo ruy2 e8))
   
   putStrLn "test 15 - white moves"
   pBrd ruy2
   print (map pMove (foldl (\acc x -> acc ++ x) [] (map (pseudo ruy2) (manOn ruy2 White))))
   
   putStrLn "test 16 - black moves"
   print (map pMove (foldl (\acc x -> acc ++ x) [] (map (pseudo ruy2) (manOn ruy2 Black))))
   
   putStrLn "test 17 - attack"
   pBrd ruy3
   print (attacks ruy3 e8 White)
   print (attacks ruy3 e8 Black)
   print (attacks ruy3 a1 White)
   print (attacks ruy3 a1 Black)
   
   putStrLn "test 18 - black attack squares"
   pBrd brd0
   putStr "Black attacks e4 = "; print (attacks brd0 e4 Black)
   putStr "White attacks e4 = "; print (attacks brd0 e4 White)
   putStr "White attacks e3 = "; print (attacks brd0 e3 White)
   
   putStrLn "test 19 - black attack squares"
   pBrd ruy3
   putStr "White attacks e8 = "; print (attacks ruy3 e8 White)
   putStr "White attacks d8 = "; print (attacks ruy3 d8 White)
   putStr "White attacks a8 = "; print (attacks ruy3 a8 White)
   putStr "White attacks e5 = "; print (attacks ruy3 e5 White)
   
   putStrLn "test 20 - check"
   putStr "White in check ="; print (inCheck ruy3 White)
   putStr "Black in check ="; print (inCheck ruy3 Black)
   
   putStrLn "test 21 - King trials"
   pBrd [(wK,e1),(bK,e8)]
   putStr "Trial moves by White = "; print (map pMove (trials [(wK,e1),(bK,e8)] White))
   putStr "Trial moves by Black = "; print (map pMove (trials [(wK,e1),(bK,e8)] Black))
   
   putStrLn "test 21 - King + Queen trials"
   pBrd brdRK
   putStr "Trial moves by White = "; print (map pMove (trials brdRK White))
   pBrd [(wK,e1),(bK,e8),(wQ,d1),(bQ,d8)]
   putStr "Trial moves by White = "; print (map pMove (trials [(wK,e1),(bK,e8),(wQ,d1),(bQ,d8)] White))
   putStr "Trial moves by Black = "; print (map pMove (trials [(wK,e1),(bK,e8),(wQ,d1),(bQ,d8)] Black))      
   pBrd [(wK,e1),(bK,e8)]   
   pBrd brd0
   putStr "Trial moves by White = "; print (map pMove (trials brd0 White))
   
   putStrLn "test 22 - Black in Check"
   pBrd ruy3
   putStrLn "Trial moves by Black = "; print (map pMove (trials ruy3 Black))
   putStr "Black in check = "; print (inCheck ruy3 Black)
   putStr "White in check = "; print (inCheck ruy3 White)   
   putStr "Legal moves by Black = "; print (map pMove (legals ruy3 Black))
   
   putStrLn "test 23 - Minimax"
   pBrd brd0
   putStr "minimax 0 White brd0 = "; print (minimax 0 White brd0)
   putStr "minimax 1 White brd0 = "; print (minimax 1 White brd0)
   
   putStrLn "test 23 - Minimax sees capture"   
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
   
   putStrLn "test 24 - Scored Moves"
   let brd24 = [(wK,e1),(bK,e8),(bQ,e2)]
   pBrd brd24
   putStr "scoreMoves brd24 White = "; print (map psmv (scoreMoves brd24 White))
   
   putStrLn "test 25 - Scored Moves"
   let brd25 = [(wK,e1),(bK,e8),(bQ,a2)]
   pBrd brd25
   putStr "scoreMoves brd25 White = "; print (map psmv (scoreMoves brd25 White))
   
   putStrLn "test 26 - Scored Moves"
   let brd26 = [(wK,e1),(bK,d3),(bQ,a2)]
   pBrd brd26
   putStr "scoreMoves brd26 White = "; print (map psmv (scoreMoves brd26 White))

   putStrLn "test 27 - Scored Moves"
   let brd27 = [(wK,e1),(bK,d3),(bQ,a2),(wN,c3)]
   pBrd brd27
   putStr "scoreMoves brd27 White = "; print (map psmv (scoreMoves brd27 White))
   putStr "bestScoreMove brd27 White = "; print (psmv (bestScoreMove brd27 White))
   putStr "scoreMoves brd27 Black = "; print (map psmv (scoreMoves brd27 Black))   
   putStr "bestScoreMove brd27 Black = "; print (psmv (bestScoreMove brd27 Black))

   putStrLn "test 28 - Alphabeta"
   pBrd brd0
   putStr "alphabeta brd0 0 (minBound::Int) (maxBound::Int) White = "; print (alphabeta brd0 0 (minBound::Int) (maxBound::Int) White)
   putStr "alphabeta brd0 1 (minBound::Int) (maxBound::Int) White = "; print (alphabeta brd0 1 (minBound::Int) (maxBound::Int) White)
   putStr "alphabeta brd0 2 (minBound::Int) (maxBound::Int) White = "; print (alphabeta brd0 2 (minBound::Int) (maxBound::Int) White)

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