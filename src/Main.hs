{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Game
import Raylib.Core (closeWindow, initWindow, setTargetFPS, windowShouldClose)
import Raylib.Util (raylibApplication)

startup :: IO Game
startup = do
  window <- initWindow width height "raylib example - basic window"
  setTargetFPS frameRate
  return (defaultState window)

width, height :: (Num a) => a
width = 1000
height = 800

frameRate :: (Num a) => a
frameRate = 60

shouldClose :: Game -> IO Bool
shouldClose = const windowShouldClose

teardown :: Game -> IO ()
teardown = closeWindow . game'window

mainLoop :: Game -> IO Game
mainLoop gameState = do
  drawGame gameState
  return (updateGame gameState)

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
