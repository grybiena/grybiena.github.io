module Main where

import Prelude hiding (top)
import Effect (Effect)
import Examples.Halogen.XTerm.Component as XTerm
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    void $ runUI XTerm.component unit body

