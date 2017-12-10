------------------------------------------------------------------------------------------
-- User Command Loop
------------------------------------------------------------------------------------------

module Main

where

--import Data.Char
--import Control.Monad
import System.IO
import Square

main = intro

intro = do
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

--dispatch brd side "test" = do tests
--                              command brd side

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



