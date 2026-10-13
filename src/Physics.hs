module Physics (Kinematics (..), Object (..), Active (..), Receive (..), updateActiveObjects, updateReceiveObjects) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Lens.Micro ((^.))
import Linear.V2 (V2 (V2), _x, _y)
import Raylib.Core.Shapes (checkCollisionRecs)
import Raylib.Types (Color, Rectangle (Rectangle), Vector2)
import Shape (PRS (..), Shape (..), checkCollisionShape, simplifyPRS)

data Kinematics = Kinematics
  { kinematics'PRS :: PRS,
    kinematics'VelocityPRS :: PRS,
    kinematics'AccelerationPRS :: PRS
  }

data Object = Object
  { object'shape :: Shape Float,
    object'kinematics :: Kinematics,
    object'color :: Color
  }

data Active a = Active
  { active'object :: a,
    active'update :: a -> a
  }

data Receive a = Receive
  { receive'object :: a,
    receive'update :: a -> a -> a
  }

updateActive :: Active a -> Active a
updateActive Active {active'object = object, active'update = objectUpdate} =
  Active {active'object = objectUpdate object, active'update = objectUpdate}

updateActiveObjects :: Vector (Active a) -> Vector (Active a)
updateActiveObjects = V.map updateActive

updateReceive :: a -> Receive a -> Receive a
updateReceive
  otherObject
  Receive {receive'object = object, receive'update = objectUpdate} =
    Receive {receive'object = objectUpdate otherObject object, receive'update = objectUpdate}

findCollisionObjects :: Object -> Vector Object -> Vector Object
findCollisionObjects
  Object {object'shape = shape1, object'kinematics = Kinematics {kinematics'PRS = prs1}}
  objects =
    ( V.filter $
        \Object {object'shape = shape2, object'kinematics = Kinematics {kinematics'PRS = prs2}} ->
          checkCollisionShape (simplifyPRS prs1 prs2) shape1 shape2
    )
      $ objects

updateReceiveObjects :: Vector (Receive Object) -> Vector Object -> Vector (Receive Object)
updateReceiveObjects receiveObjects objects =
  V.map
    (\obj -> V.foldr updateReceive obj $ findCollisionObjects (receive'object obj) objects)
    receiveObjects
