module Shape (Shape (..), checkCollisionShape, drawShape, checkCollisionPolyPoly, aabbPoly, nearestPointShape) where

import Data.Foldable (minimumBy)
import Data.Function (on)
import Lens.Micro ((^.))
import Linear.V2 (V2 (V2), perp, _x, _y)
import Linear.Vector ((^*), (^-^))
import Raylib.Core.Shapes (checkCollisionCircleRec, checkCollisionCircles, checkCollisionRecs, drawCircleV, drawRectangleV)
import Raylib.Types (Color, Rectangle (Rectangle), Vector2)
import Raylib.Util.Math (Vector (magnitude, vectorNormalize), clamp, (|.|))

data Shape a
  = Circ {_radius :: a}
  | Rect {_dimension :: V2 a}

checkCollisionShape :: Vector2 -> Shape Float -> Shape Float -> Bool
checkCollisionShape (V2 posX posY) s1 s2 = case (s1, s2) of
  (Circ r1, Circ r2) -> checkCollisionCircles (V2 posX posY) r1 (V2 0 0) r2
  (Rect (V2 w1 h1), Rect (V2 w2 h2)) ->
    checkCollisionRecs
      (Rectangle posX posY w1 h1)
      (Rectangle posX posY w2 h2)
  (Circ r, Rect (V2 w h)) ->
    checkCollisionCircleRec (V2 posX posY) r (Rectangle 0 0 w h)
  (Rect (V2 w h), Circ r) ->
    checkCollisionCircleRec (V2 0 0) r (Rectangle posX posY w h)

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

drawShape :: Vector2 -> Shape Float -> Color -> IO ()
drawShape pos shape = case shape of
  Circ r -> drawCircleV pos r
  Rect dim -> drawRectangleV pos dim
