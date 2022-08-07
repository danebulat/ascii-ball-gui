{-# LANGUAGE TemplateHaskell #-}

module Animation where

import Control.Concurrent ( threadDelay )
import Control.Lens
import Data.Default
import Data.Text          ( Text )
import Data.Time
import Monomer

import qualified Data.Text as T
import qualified Monomer.Lens as L

import AppTypes 

-- -------------------------------------------------------------------
-- Build UI

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    -- variables
    containerBg = rgba 0 0 0 0.2
    containerBorder = border 1.0 black
    
    -- get current time from model as string 
    timeString = T.pack . show $ model ^. currentTime

    -- time label
    timeLabel = label (T.takeWhile (/= '.') timeString)
      `styleBasic` [textFont "Bold", textColor white, textSize 18, textLeft, textTop, flexHeight 100]

    -- main ui
    widgetTree = zstack [
      -- patterned background
      image_ "./assets/images/pattern09.png" [fitFill, imageRepeatX, imageRepeatY],
      
      zstack [
          -- outer-most hstack to hold box containers
          hstack_ [childSpacing_ 10] [

            -- left vstack
            vstack_ [childSpacing_ 10] [
                label "Ascii Ball GUI" `styleBasic` [textFont "Bold", textSize 20],
                
                animFadeIn_ [duration 250] timeLabel
                  `nodeKey` "fadeTimeLabel"
                  `styleBasic` [flexWidth 100]
              ] `styleBasic` [bgColor containerBg, padding 10, containerBorder],

            -- right vstack
            vstack [
                label "animation here..." `styleBasic` [textCenter, textMiddle, flexHeight 100]
                  `styleBasic` [flexWidth 100]
              ] `styleBasic` [bgColor containerBg, padding 10, containerBorder]
            ]
        ] `styleBasic` [padding 10]
      ]

-- -------------------------------------------------------------------
-- Event handler

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt =
  case evt of
    -- start time producer
    AppInit -> [Producer timeOfDayProducer]

    -- update time in model
    AppSetTime time -> fadeInMsg time ++ [Model $ model & currentTime .~ time]
  where
    fadeInMsg time
      -- todSec converts time to seconds
      | truncate (todSec time) `mod` 10 /= 0 = []
      | otherwise = [Message "fadeTimeLabel" AnimationStart]

-- -------------------------------------------------------------------
-- Producers

timeOfDayProducer :: (AppEvent -> IO ()) -> IO ()
timeOfDayProducer sendMsg = do
  time <- getLocalTimeOfDay   -- get current time
  sendMsg (AppSetTime time)   -- broadcast AppSetTime event to add
  threadDelay $ 1000 * 1000   -- delays thread for 1 second
  timeOfDayProducer sendMsg   -- re-call function recursively

getLocalTimeOfDay :: IO TimeOfDay
getLocalTimeOfDay = do
  time <- getZonedTime        -- IO ZonedTime
  return
    . localTimeOfDay          -- LocalTime -> TimeOfDay
    . zonedTimeToLocalTime    -- ZonedTime -> LocalTime (a local time together with a time zone)
    $ time                    -- IO TimeOfDay 

-- -------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  time <- getLocalTimeOfDay
  startApp (model time) handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Ascii Ball GUI"
      , appWindowIcon  "./assets/images/icon.png"
      , appTheme       darkTheme
      , appFontDef     "Regular" "./assets/fonts/Roboto-Regular.ttf"
      , appFontDef     "Bold"    "./assets/fonts/Roboto-Bold.ttf"
      , appInitEvent   AppInit ]
    model time =
      let m = def :: AppModel
      in m {_currentTime = time }
