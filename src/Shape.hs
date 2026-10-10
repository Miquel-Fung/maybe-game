module Shape (PRS (..), Shape (..), checkCollisionShape, drawShape, checkCollisionPolyPoly, aabbPoly, nearestPointShape) where

import Data.Foldable (minimumBy)
import Data.Function (on)
import Lens.Micro ((^.))
import Linear.V2 (V2 (V2), perp, _x, _y)
import Linear.Vector ((^*), (^-^))
import Raylib.Core.Shapes (checkCollisionCircleRec, checkCollisionCircles, checkCollisionRecs, drawCircleV, drawRectangleV, drawTriangle)
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

checkCollisionShape :: PRS -> Shape Float -> Shape Float -> Bool
checkCollisionShape (PRS (V2 x y) 0 s) (Rect (V2 w1 h1)) (Rect (V2 w2 h2)) =
  checkCollisionRecs
    (Rectangle (x * s) (y * s) w1 h1)
    (Rectangle 0 0 w2 h2)
checkCollisionShape (PRS p _ s) (Circ r1) (Circ r2) =
  checkCollisionCircles p (r1 * s) (V2 0 0) (r2 * s)
checkCollisionShape (PRS p _ s) (Circ r) (Rect (V2 w h)) =
  checkCollisionCircleRec p (r * s) (Rectangle 0 0 w h)
checkCollisionShape (PRS (V2 x y) rot s) (Rect (V2 w1 h1)) (Rect (V2 w2 h2)) =
  let point = V2 x y
      width = vector2Rotate (V2 (w1 * s) 0) rot
      height = vector2Rotate (V2 0 (h1 * s)) rot
   in checkCollisionPolyPoly
        [point, point + width, point + height, point + width + height]
        [V2 0 0, V2 w2 0, V2 0 h2, V2 w2 h2]
checkCollisionShape (PRS p rot s) (Rect dim) (Circ r) =
  checkCollisionShape
    (PRS (vector2Rotate (-p) (-rot)) 0 1)
    (Circ r)
    (Rect $ dim ^* s)

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

linesPoly :: [a] -> [(a, a)]
linesPoly (p : ps) = zip (p : ps) (ps ++ [p])
linesPoly [] = []

-- convex only
checkCollisionPolyPoly :: [Vector2] -> [Vector2] -> Bool
checkCollisionPolyPoly as bs =
  let project = map (\x -> map (\y -> x |.| y) bs) . normalsPoly
      minA = map minimum $ project as
      maxA = map maximum $ project as
      minB = map minimum $ project bs
      maxB = map maximum $ project bs
   in (and $ zipWith (>) maxA minB) && (and $ zipWith (>) maxB minA)

normalsPoly :: [Vector2] -> [Vector2]
normalsPoly ps = map (\(a, b) -> negate . perp . vectorNormalize $ b ^-^ a) $ linesPoly ps

aabbPoly :: [Vector2] -> Rectangle
aabbPoly ps =
  let xs = map (^. _x) ps
      ys = map (^. _y) ps
   in Rectangle (minimum xs) (minimum ys) (maximum xs - minimum xs) (maximum ys - minimum ys)

drawShape :: PRS -> Shape Float -> Color -> IO ()
drawShape (PRS pos _ scale) (Circ radius) = drawCircleV pos (radius * scale)
drawShape (PRS pos 0 scale) (Rect dim) = drawRectangleV pos $ dim ^* scale
drawShape (PRS pos rot s) (Rect (V2 w h)) = \color ->
  let width = vector2Rotate (V2 (w * s) 0) rot
      height = vector2Rotate (V2 0 (h * s)) rot
   in drawTriangle (pos + height) (pos + width) pos color
        >> drawTriangle (pos + height) (pos + width + height) (pos + width) color
