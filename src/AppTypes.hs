{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module AppTypes where

import Control.Lens.TH
import Data.Default 
import Data.Time ( TimeOfDay )
import Data.Text ( Text )

-- -------------------------------------------------------------------
-- Animation types 

data Vector = Vector
  { _getX :: Int
  , _getY :: Int
  } deriving (Eq, Show)

data AnimationState = AnimationState
  { _ballVelocity :: Vector
  , _ballPosition :: Vector
  } deriving (Eq, Show)

-- -------------------------------------------------------------------
-- Lenses 

data RenderChars = RenderChars
  { _verticalWallChar   :: Char
  , _horizontalWallChar :: Char
  , _cornerChars        :: (Char, Char)
  , _ballChar           :: Char
  } deriving (Eq, Show)

data AppModel = AppModel
  { _ballInitialVelocity :: Vector
  , _ballInitialPosition :: Vector
  , _frameWidth          :: Int
  , _frameHeight         :: Int
  , _renderChars         :: RenderChars
  , _currentTime         :: TimeOfDay
  , _renderString        :: Text
  , _animationState      :: AnimationState
  } deriving (Eq, Show)

makeLenses 'Vector
makeLenses 'AnimationState
makeLenses 'RenderChars
makeLenses 'AppModel

-- -------------------------------------------------------------------
-- Events

data AppEvent
  = AppInit
  | AppSetTime TimeOfDay
  | RenderAnimation
  | DoNothing Int
  | UpdatePosX
  | AppReset
  | AppExit
  deriving (Eq, Show)

-- -------------------------------------------------------------------
-- Defaults

instance Default Vector where
  def = Vector 0 0

instance Default RenderChars where
  def = RenderChars
    { _verticalWallChar   = '|'
    , _horizontalWallChar = '-'
    , _cornerChars        = ('/', '\\')
    , _ballChar           = 'x'
    }

instance Default AppModel where
  def = AppModel
    { _ballInitialVelocity = Vector 1 1
    , _ballInitialPosition = Vector 3 4
    , _frameWidth          = 20
    , _frameHeight         = 10
    , _renderChars         = def :: RenderChars
    , _currentTime         = undefined  -- put getLocalTimeOfDay in module and call here
    , _renderString        = ""
    , _animationState      = AnimationState (Vector 1 1) (Vector 3 4)
    }
