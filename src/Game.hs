module Game where

import Raylib.Types (Camera2D, Color, Vector2)
import Shape

data StaticEntity = StaticEntity
  { staticEntity'shape :: Shape Float,
    staticEntity'color :: Color
  }

data Entity = Entity
  { entity'shape :: Shape Float
  }

data Player = Player {}

data Game = Game
  { game'player :: Player,
    game'camera :: Camera2D,
    game'level :: Level
  }

data Level = Level
  {
  }
