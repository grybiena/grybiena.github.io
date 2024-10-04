module Main where

import Prelude hiding (top)
import Effect (Effect)
import Examples.Halogen.Canvas.Sketch as Canvas 
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    void $ runUI Canvas.component unit body

