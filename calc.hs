-- calculator example
--
-- for documentation on Gtk library functions see
-- 

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

main :: IO ()
main = do
  initGUI
  window <- windowNew
  hbox   <- hBoxNew True 10
 -- button1 <- buttonNewWithLabel "Button 1"
 -- button2 <- buttonNewWithLabel "Button 2"  
  set window [ windowTitle := "Calculator", windowResizable := False,
               windowDefaultWidth  := 230, windowDefaultHeight := 250,
               containerBorderWidth := 100, containerChild := hbox]
 -- boxPackStart hbox button1 PackGrow 0
 -- boxPackStart hbox button2 PackGrow 0
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
  