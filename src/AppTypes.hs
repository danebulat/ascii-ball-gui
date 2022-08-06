{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module AppTypes where

import Control.Lens.TH
import Data.Default 
import Data.Time (TimeOfDay)
import Animation (getLocalTimeOfDay)

-- -------------------------------------------------------------------
-- Animation types 

data Vector = Vector
  { getX :: Int
  , getY :: Int
  } deriving (Eq, Show)

data AnimationState = AnimationState
  { ballVelocity :: Vector
  , ballPosition :: Vector
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
  } deriving (Eq, Show)

makeLenses 'RenderChars
makeLenses 'AppModel

-- -------------------------------------------------------------------
-- Events

data AppEvent
  = AppInit
  | AppSetTime TimeOfDay
  deriving (Eq, Show)

-- -------------------------------------------------------------------
-- Defaults

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
    , _frameHeight         = 20
    , _renderChars         = def :: RenderChars
    , _currentTime         = undefined  -- put getLocalTimeOfDay in module and call here
    }
