import Control.Monad.IfElse
import FRP.Yampa as Yampa

import Game
import Display
import Input
import Graphics.UI.SDL as SDL
import Graphics.UI.Extra.SDL
import Graphics.UI.Extra.FPS

-- TODO: Use MaybeT or ErrorT to report errors
main :: IO ()
main = do

  initializeDisplay

  timeRef       <- initializeTimeRef
  controllerRef <- initializeInputDevices
  res           <- loadResources
  fpsRef        <- initialiseFPSCounter 100 (fromIntegral `fmap` SDL.getTicks)

  awhen res $ \res' -> do
    reactimate (initGraphs >> senseInput controllerRef)
               (\_ -> do
                  -- Get clock and new input
                  dtSecs <- fmap milisecsToSecs $ senseTimeRef timeRef
                  mInput <- senseInput controllerRef
                  return (dtSecs, Just mInput)
               )
               (\_ e -> stepFPSCounter fpsRef >> render res' e >> return False)

               wholeGame
