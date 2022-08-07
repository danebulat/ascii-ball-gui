module Animation where

import AppTypes

ballChar' :: RenderChars -> Char
ballChar' = _ballChar

cornerChars' :: RenderChars -> (Char, Char)
cornerChars' = _cornerChars

verticalWallChar' :: RenderChars -> Char
verticalWallChar' = _verticalWallChar 

horizontalWallChar' :: RenderChars -> Char
horizontalWallChar' = _horizontalWallChar 

-- -------------------------------------------------------------------
-- Animation logic

mkState :: Vector -> Vector -> AnimationState
mkState = AnimationState

nextStateX :: AnimationState -> Vector -> AnimationState
nextStateX AnimationState
             { _ballVelocity = Vector vX vY
             , _ballPosition = Vector pX pY }
           Vector
             { _getX = w
             , _getY = y }
  | pX+vX > w-2 =
    let newX = calcOvershoot (w-2) pX vX
    in mkState (Vector (negate vX) vY) (Vector newX pY)
    
  | pX+vX < 1 =
    let newX = calcOvershoot' 1 pX vX
    in mkState (Vector (negate vX) vY) (Vector newX pY)
    
  | otherwise = mkState (Vector vX vY) (Vector (pX+vX) pY)

nextStateY :: AnimationState -> Vector -> AnimationState
nextStateY AnimationState
             { _ballVelocity = Vector vX vY
             , _ballPosition = Vector pX pY }
           Vector
             { _getX = w
             , _getY = h }
  | pY+vY > h-3 =
    let  newY = calcOvershoot (h-3) pY vY
    in mkState (Vector vX (negate vY)) (Vector pX newY)
    
  | pY+vY < 1 =
    let newY = calcOvershoot' 1 pY vY
    in mkState (Vector vX (negate vY)) (Vector pX newY)
    
  | otherwise= mkState (Vector vX vY) (Vector pX (pY+vY))

calcOvershoot :: Int -> Int -> Int -> Int
calcOvershoot bound p v =
  let ov = abs((p + v) - bound) -- overshoot in next frame
      tw = bound - p            -- space to wall
      mv = abs (ov - tw)        -- amount to move in opposite direction from wall
  in bound - mv

calcOvershoot' :: Int -> Int -> Int -> Int
calcOvershoot' bound p v =
  let ov = abs((p + v) + bound) -- overshoot in next frame
      tw = bound + p            -- space to wall
      mv = abs (ov - tw)        -- amount to move in opposite direction from wall
  in bound + mv

-- -------------------------------------------------------------------
-- Render logic

render :: AppModel -> AnimationState -> String
render AppModel
         { _frameWidth   = width
         , _frameHeight  = height
         , _renderChars  = rchars }
       AnimationState
         { _ballPosition = pos } =

  -- Calculate total characters to draw on screen
  -- https://stackoverflow.com/questions/1730961/convert-a-2d-array-index-into-a-1d-index
  let height'    = height - 1
      bufferSize = (width * height')
      Vector x y = pos
      ballPos    = (y * width) + x    -- OR (row * row_width) + col for flipped axes

  -- Recursively construct string to draw to screen
  -- Remove 'init' to align correctly in GHCI
  in go 0 bufferSize ballPos
  where
    go :: Int -> Int -> Int -> [Char]
    go i target ballPos

      -- end of buffer
      | i > target = []

      -- draw ball
      | i == ballPos = ballChar' rchars : go (i+1) target ballPos

      -- draw corners
      | i == 0 || i == target-1 =
        fst (cornerChars' rchars) : go (i+1) target ballPos

      | i == (width-1) || i == (target-width) =
        snd (cornerChars' rchars) :  go (i+1) target ballPos

      -- draw vertical walls 
      | i `rem` width == 0 && (i < (width * (height-1))) ||
        i `rem` width == (width-1) =
          verticalWallChar' rchars: go (i+1) target ballPos

      -- draw horizontal wall
      | i `elem` [1..width-1] ++
                 [(width*(height-1))-width..(width*(height-1))]
        && (i < (width*(height-1))) =
          horizontalWallChar' rchars : go (i+1) target ballPos

      -- default draw empty space 
      | otherwise = ' ' : go (i+1) target ballPos

