module Draw where

import Data.Vector (Vector)
import Physics
import Shape

drawObject :: Object -> IO ()
drawObject
  ( Object
      { object'shape = shape,
        object'kinematics = Kinematics {kinematics'PRS = prs},
        object'color = color
      }
    ) = drawShape prs shape color

drawObjects :: Vector Object -> IO ()
drawObjects objects = mapM_ drawObject objects
