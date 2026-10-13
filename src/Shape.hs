module Shape
  ( PRS (..),
    Shape (..),
    checkCollisionShape,
    drawShape,
    nearestPointShape,
    simplifyPRS,
  )
where

import Data.Foldable (minimumBy)
import Data.Function (on)
import Lens.Micro ((^.))
import Linear.V2 (V2 (V2), perp, _x, _y)
import Linear.Vector ((^*), (^-^), (^/))
import Raylib.Core.Shapes
  ( checkCollisionCircleRec,
    checkCollisionCircles,
    checkCollisionPointPoly,
    checkCollisionRecs,
    drawCircleV,
    drawRectangleV,
    drawTriangle,
  )
import Raylib.Types (Color, Rectangle (Rectangle), Vector2)
import Raylib.Util.Math (Vector (magnitude, vectorNormalize), clamp, vector2Rotate, (|.|))

data Shape a
  = Circ {circ'radius :: a}
  | Rect {rect'dimension :: V2 a}

data PRS = PRS
  { prs'position :: Vector2,
    prs'rotation :: Float,
    prs'scale :: Float
  }
  deriving (Show)

simplifyPRS :: PRS -> PRS -> PRS
simplifyPRS
  (PRS {prs'position = pos1, prs'rotation = rot1, prs'scale = scale1})
  (PRS {prs'position = pos2, prs'rotation = rot2, prs'scale = scale2}) =
    PRS
      { prs'position = (vector2Rotate (pos1 - pos2) (-rot2)) ^/ scale2,
        prs'rotation = rot1 - rot2,
        prs'scale = scale1 / scale2
      }

checkCollisionShape :: PRS -> Shape Float -> Shape Float -> Bool
checkCollisionShape (PRS (V2 x y) 0 s) (Rect (V2 w1 h1)) (Rect (V2 w2 h2)) =
  checkCollisionRecs
    (Rectangle x y (w1 * s) (h1 * s))
    (Rectangle 0 0 w2 h2)
checkCollisionShape (PRS p rot s) shape1 shape2 = case (shape1, shape2) of
  (Circ r1, Circ r2) ->
    checkCollisionCircles p (r1 * s) (V2 0 0) (r2 * s)
  (Circ radius, Rect (V2 w h)) ->
    checkCollisionCircleRec p (radius * s) (Rectangle 0 0 w h)
  (Rect dim, Circ r) ->
    checkCollisionShape (PRS (vector2Rotate (-p) (-rot)) 0 1) (Circ r) (Rect $ dim ^* s)
  (Rect (V2 w1 h1), Rect (V2 w2 h2)) ->
    let width = vector2Rotate (V2 (w1 * s) 0) rot
        height = vector2Rotate (V2 0 (h1 * s)) rot
     in checkCollisionPolyPoly
          [p, p + width, p + width + height, p + height]
          [0, V2 w2 0, V2 w2 h2, V2 0 h2]

checkCollisionPolyPoly :: [Vector2] -> [Vector2] -> Bool
checkCollisionPolyPoly vec1 vec2 =
  or (map (flip checkCollisionPointPoly vec2) vec1)
    || or (map (flip checkCollisionPointPoly vec1) vec2)

nearestPointShape :: Vector2 -> Shape Float -> Vector2
nearestPointShape pos shape = case shape of
  Circ r -> (vectorNormalize (pos) ^* r)
  Rect (V2 w h) ->
    let cX = clamp 0 w $ pos ^. _x
        cY = clamp 0 h $ pos ^. _y
     in minimumBy
          (compare `on` magnitude . (^-^ pos))
          [V2 cX 0, V2 cX h, V2 0 cY, V2 w cY]

nearestPointLine :: Vector2 -> Vector2 -> Vector2 -> Vector2
nearestPointLine pos l1 l2 =
  let line = vectorNormalize $ l2 ^-^ l1
   in l1 + (line ^* clamp 0 (line |.| (l2 ^-^ l1)) (line |.| (pos ^-^ l1)))

drawShape :: PRS -> Shape Float -> Color -> IO ()
drawShape (PRS pos _ scale) (Circ radius) = drawCircleV pos (radius * scale)
drawShape (PRS pos 0 scale) (Rect dim) = drawRectangleV pos (dim ^* scale)
drawShape (PRS pos rot s) (Rect (V2 w h)) = \color ->
  let width = vector2Rotate (V2 (w * s) 0) rot
      height = vector2Rotate (V2 0 (h * s)) rot
   in drawTriangle (pos + height) (pos + width) pos color
        >> drawTriangle (pos + height) (pos + width + height) (pos + width) color
