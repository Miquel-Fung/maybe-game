module Game where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Draw
import Linear.V2 (V2 (V2))
import Physics (Active (..), Kinematics (..), Object (..), Receive (..), updateActiveObjects, updateReceiveObjects)
import Raylib.Core (clearBackground)
import Raylib.Types (Camera2D (Camera2D), Color)
import Raylib.Util (WindowResources, drawing)
import Raylib.Util.Colors (blue, green, red, white)
import Shape

addPRS :: PRS -> PRS -> PRS
addPRS
  (PRS {prs'position = pos1, prs'rotation = rot1, prs'scale = scale1})
  (PRS {prs'position = pos2, prs'rotation = rot2, prs'scale = scale2}) =
    PRS
      { prs'position = pos1 + pos2,
        prs'rotation = rot1 + rot2,
        prs'scale = scale1 + scale2
      }

updateKinematics :: Kinematics -> Kinematics
updateKinematics
  Kinematics
    { kinematics'PRS = prs,
      kinematics'velocityPRS = prs',
      kinematics'accelerationPRS = prs''
    } =
    Kinematics
      { kinematics'PRS = (addPRS prs $ addPRS prs' prs''),
        kinematics'velocityPRS = addPRS prs' prs'',
        kinematics'accelerationPRS = PRS {prs'position = 0, prs'rotation = 0, prs'scale = 0}
      }

data Player = Player (Active (Receive Object))

data Game = Game
  { game'window :: Maybe WindowResources,
    game'player :: Player,
    game'camera :: Camera2D,
    game'level :: Level
  }

data Level = Level
  { level'background :: Color,
    level'objects :: Vector Object,
    level'activeObjects :: Vector (Active Object),
    level'receiveObjects :: Vector (Receive Object)
  }

updateLevel :: Level -> Level
updateLevel
  Level
    { level'background = background,
      level'objects = objects,
      level'activeObjects = activeObjects,
      level'receiveObjects = receiveObjects
    } =
    Level
      { level'background = background,
        level'objects = objects,
        level'activeObjects = updateActiveObjects activeObjects,
        level'receiveObjects =
          foldl'
            updateReceiveObjects
            receiveObjects
            [ objects,
              V.map active'object activeObjects
            ]
      }

updateGame :: Game -> Game
updateGame
  Game
    { game'window = window,
      game'player = player,
      game'camera = camera,
      game'level = level
    } =
    Game
      { game'window = window,
        game'player = player,
        game'camera = camera,
        game'level = updateLevel level
      }

drawGame :: Game -> IO ()
drawGame gameState = drawing $ do
  let level = game'level gameState
  clearBackground $ level'background level
  drawObjects $ level'objects level
  drawObjects . (V.map active'object) $ level'activeObjects level
  drawObjects . (V.map receive'object) $ level'receiveObjects level

defaultState :: WindowResources -> Game
defaultState window =
  Game
    { game'window = Just window,
      game'player = Player {},
      game'camera = Camera2D {},
      game'level =
        Level
          { level'background = white,
            level'objects =
              V.fromList
                [],
            level'activeObjects =
              V.fromList
                [ Active
                    { active'object =
                        Object
                          { object'shape = Rect (V2 150 100),
                            object'kinematics =
                              Kinematics
                                { kinematics'PRS = PRS {prs'position = V2 350 350, prs'rotation = pi / 8, prs'scale = 1.4},
                                  kinematics'accelerationPRS = PRS {prs'position = 0, prs'rotation = pi / 60, prs'scale = 0}
                                },
                            object'color = blue
                          },
                      active'update =
                        \Object
                           { object'shape = shape,
                             object'kinematics = Kinematics {kinematics'PRS = prs, kinematics'accelerationPRS = accelerationPRS},
                             object'color = color
                           } ->
                            Object
                              { object'shape = shape,
                                object'kinematics =
                                  Kinematics
                                    { kinematics'PRS = addPRS prs accelerationPRS,
                                      kinematics'accelerationPRS = accelerationPRS
                                    },
                                object'color = color
                              }
                    }
                ],
            level'receiveObjects =
              V.fromList
                [ Receive
                    { receive'object =
                        Object
                          { object'shape = Rect (V2 80 40),
                            object'kinematics =
                              Kinematics
                                { kinematics'PRS = PRS {prs'position = V2 300 300, prs'rotation = 0, prs'scale = 2},
                                  kinematics'accelerationPRS = PRS {prs'position = 0, prs'rotation = pi / 120, prs'scale = 0}
                                },
                            object'color = green
                          },
                      receive'update =
                        \_
                         Object
                           { object'shape = shape,
                             object'kinematics =
                               Kinematics
                                 { kinematics'PRS = prs,
                                   kinematics'accelerationPRS = acceleration
                                 },
                             object'color = color
                           } ->
                            Object
                              { object'shape = shape,
                                object'kinematics =
                                  Kinematics
                                    { kinematics'PRS = addPRS prs acceleration,
                                      kinematics'accelerationPRS = acceleration
                                    },
                                object'color = if (color /= red) then red else green
                              }
                    }
                ]
          }
    }
