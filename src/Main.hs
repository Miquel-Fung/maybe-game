{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified HsLua ()
import Linear
import Raylib.Core (beginDrawing, clearBackground, closeWindow, endDrawing, initWindow, windowShouldClose)
import Raylib.Core.Text (drawText)
import Raylib.Util.Colors (blue, green, lightGray, red, white)
import Shape

main :: IO ()
main = do
  window <- initWindow 800 450 "raylib example - basic window"
  render
  closeWindow $ Just window

render :: IO ()
render = do
  beginDrawing
  clearBackground white
  drawText "Congrats! You created your first window!" 190 200 20 lightGray
  let rectPRS = PRS (V2 100 100) (pi / 4) 2
      circPRS = PRS (V2 250 200) 0 1
  drawShape rectPRS (Rect (V2 80 40)) (if checkCollisionShape (PRS (V2 (100 - 250) (100 - 200)) (pi / 4) 2) (Rect (V2 80 40)) (Circ 50) then red else green)
  drawShape circPRS (Circ 50) blue
  endDrawing
  windowShouldClose >>= (\x -> if x then return () else render)
