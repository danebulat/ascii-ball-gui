{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings  #-}

module GUI where

import Control.Concurrent ( threadDelay )
import Control.Lens
import Data.Default
import Data.Text          ( Text )
import Data.Time
import TextShow           ( showt )
import Monomer

import qualified Data.Text as T
import qualified Monomer.Lens as L

import AppTypes
import AppAnimation

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
      `styleBasic` [textFont "HackRegular", textColor white, textSize 18, textLeft, textTop]

    -- main ui
    widgetTree = zstack [
      -- patterned background
      image_ "./assets/images/pattern09.png" [fitFill, imageRepeatX, imageRepeatY],

      zstack [
          -- outer-most hstack to hold box containers
          hstack_ [childSpacing_ 10] [

            -- left vstack
            vstack_ [childSpacing_ 10] [

                -- title 
                label "Ascii Ball GUI" `styleBasic` [textFont "Bold", textSize 20],

                -- current time 
                animFadeIn_ [duration 250] timeLabel
                  `nodeKey` "fadeTimeLabel",

                -- input fields
                spacer,
                hgrid [ label "PosX:", numericField_ (animationState . ballPosition . getX) [readOnly]],
                hgrid [ label "PosY:", numericField_ (animationState . ballPosition . getY) [readOnly]],

                hgrid [ label "VelX:", numericField_ (animationState . ballVelocity . getX) [readOnly]],
                hgrid [ label "VelY:", numericField_ (animationState . ballVelocity . getY) [readOnly]],

                -- Filler to bottom of container
                filler,

                -- exit button
                hstack_ [childSpacing_ 10] [
                  mainButton_ "Start" AppStart [onClick DisableStartButton]
                    `nodeEnabled` model ^. startBtnEnabled
                    `styleBasic` [flexWidth 100],

                  button "Exit" AppExit `styleBasic` [flexWidth 100]
                ]
              ] `styleBasic` [flexWidth 100, bgColor containerBg, padding 10, containerBorder],

            -- right vstack
            vstack [
                label "Press Start"
                  `nodeVisible` model ^. startBtnEnabled
                  `styleBasic` [textCenter, textMiddle, flexHeight 50],

                label_  (model ^. renderString) [multiline]
                  `nodeVisible` not (model ^. startBtnEnabled)
                  `styleBasic` [textFont "HackRegular", textSize 12, textCenter, textMiddle, flexHeight 100]
              ] `styleBasic` [bgColor containerBg, padding 10, containerBorder, flexWidth 100]
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

    -- updates animation
    RenderAnimation ->
      let width_height     = Vector (model ^. frameWidth) (model ^. frameHeight)
          anim_state       = model ^. animationState
          new_string       = render model anim_state
          next_anim_state  = nextStateY (nextStateX anim_state width_height) width_height
          -- Add newlines to output string
          new_string'      = addNewlines new_string

      in [ Model $ model & renderString   .~ T.pack new_string'
                         & animationState .~ next_anim_state
         ]

    -- update position
    UpdatePosX ->
      [Task $ DoNothing <$>
          --((putStrLn $ show (model ^. animationState . ballPosition)) >> return 0)
          ((putStrLn $ T.unpack $ model ^. renderString) >> return 0)]

    DoNothing _ -> []

    -- reset animation settings
    AppStart -> [Producer renderProducer]

    -- disable start button after clicked
    DisableStartButton -> [ Model $ model & startBtnEnabled .~ False ]

    -- exit app
    AppExit -> [exitApplication]
  where
    fadeInMsg time
      -- todSec converts time to seconds
      | truncate (todSec time) `mod` 10 /= 0 = []
      | otherwise = [Message "fadeTimeLabel" AnimationStart]

-- -------------------------------------------------------------------
-- String processing

-- newline in between: ||, \|, |\
addNewlines :: String -> String
addNewlines []  = []
addNewlines [x] = [x]
addNewlines (x:y:xs)
  | [x,y] == "||"  = "|\n|"  ++ addNewlines xs
  | [x,y] == "\\|" = "\\\n|" ++ addNewlines xs
  | [x,y] == "|\\" = "|\n\\" ++ addNewlines xs
  | otherwise  = x : addNewlines (y:xs)

-- -------------------------------------------------------------------
-- Producers

renderProducer ::(AppEvent -> IO ()) -> IO ()
renderProducer sendMsg = do
  threadDelay 500000
  sendMsg RenderAnimation
  renderProducer sendMsg

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
      , appFontDef     "Regular"     "./assets/fonts/Roboto-Regular.ttf"
      , appFontDef     "Bold"        "./assets/fonts/Roboto-Bold.ttf"
      , appFontDef     "HackRegular" "./assets/fonts/Hack-Regular.ttf"
      , appInitEvent   AppInit ]
    model time =
      let m = def :: AppModel
      in m {_currentTime = time }
