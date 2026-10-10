module Game where

import Raylib.Types (Camera2D, Color, Transform, Vector2)
import Shape

data Kinematics a = Kinematics
  { kinematics'x :: a,
    kinematics'dx :: a,
    kinematics'ddx :: a
  }

data StaticEntity = StaticEntity
  { staticEntity'shape :: Shape Float,
    staticEntity'prs :: PRS,
    staticEntity'color :: Color
  }

data Entity = Entity
  {entity'staticEntity :: StaticEntity}

data Player = Player {}

data Game = Game
  { game'player :: Player,
    game'camera :: Camera2D,
    game'level :: Level
  }

data Level = Level
  {
  }
