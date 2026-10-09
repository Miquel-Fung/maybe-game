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
  let circ1 = Circ (V2 220 140) 100
      circ2 = Circ (V2 50 50) 100
      rect1 = Rect (V2 300 300) (V2 500 300)
  drawShape circ1 $ if (checkCollisionShape circ1 circ2) then red else green
  drawShape circ2 green
  drawShape rect1 blue
  endDrawing
  windowShouldClose >>= (\x -> if x then return () else render)
