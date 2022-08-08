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
      `styleBasic` [textFont "Regular", textColor white, textSize 12, textRight, textTop]

    -- wrappers
    panelHeader t = label t `styleBasic` [textSize 12, textFont "Bold"]
    statsLabel  t = label t `styleBasic` [textSize 11]
    statsField  v = numericField_ v [readOnly] `styleBasic` [textSize 11, height 20]
    sliderStyle   = [height 15.0]

    -- main ui
    widgetTree = zstack [
      -- patterned background
      image_ "./assets/images/pattern09.png" [fitFill, imageRepeatX, imageRepeatY],

      zstack [
          -- outer-most hstack to hold box containers
          hstack_ [childSpacing_ 10] [

            -- left vstack
            scroll (vstack_ [childSpacing_ 10] [
                hgrid [
                  label "Ascii Ball GUI" `styleBasic` [textFont "Bold", textSize 14],
                  timeLabel
                ],

                -- input fields
                spacer,
                panelHeader "Stats",
                hgrid_ [childSpacing_ 10] [
                  statsLabel "PosX:", statsField (animationState . ballPosition . getX),
                  statsLabel "PosY:", statsField (animationState . ballPosition . getY)
                ],

                hgrid_ [childSpacing_ 10] [
                  statsLabel "VelX:", statsField (animationState . ballVelocity . getX),
                  statsLabel "VelY:", statsField (animationState . ballVelocity . getY)
                ],

                -- sliders 
                spacer,
                panelHeader "Controls",
                statsLabel $ "Frame Width: " <> showt (model ^. frameWidth),
                hslider_ frameWidthD 10 30 [onChange UpdateFrameWidth] `styleBasic` sliderStyle,
                
                statsLabel $ "Frame Height: " <> showt (model ^. frameHeight),
                hslider_ frameHeightD 8 45 [onChange UpdateFrameHeight] `styleBasic` sliderStyle,
                label "Ball position will reset when the frame size is changed."
                  `styleBasic` [textSize 9],

                spacer,
                statsLabel $ "VelX Multiplier: " <> showt (fromFractional (model ^. velXD) :: Int),
                hslider_ velXD 0 5 [onChange UpdateVelX] `styleBasic` sliderStyle,

                statsLabel $ "VelY Multiplier: " <> showt (fromFractional (model ^. velYD) :: Int),
                hslider_ velYD 0 5 [onChange UpdateVelY] `styleBasic` sliderStyle,

                -- Filler to bottom of container
                filler,

                -- exit button
                hstack_ [childSpacing_ 10] [
                  mainButton_ "Start" AppStart [onClick DisableStartButton]
                    `nodeEnabled` model ^. startBtnEnabled
                    `styleBasic` [flexWidth 100, textSize 12],

                  button "Exit" AppExit `styleBasic` [flexWidth 100, textSize 12]
                ]
              ] `styleBasic` [flexWidth 100, bgColor containerBg, padding 10, containerBorder]),

            -- right vstack
            vstack [
                zstack [
                animFadeOut_ [duration 250, onFinished FadeInRender] (label "Press Start"
                  `styleBasic` [textCenter, textMiddle, flexHeight 40])
                  `nodeKey` "startLabel",

                scroll (animFadeIn_ [duration 250, onFinished StartRendering]
                  (label_  (model ^. renderString) [multiline]
                    `nodeVisible` not (model ^. startBtnEnabled)
                    `styleBasic` [textFont "HackRegular",
                                  textSize 12,
                                  textCenter,
                                  textMiddle,
                                  flexHeight 100]) `nodeKey` "renderLabel")
                ]
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
    AppSetTime time -> [Model $ model & currentTime .~ time]

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

    UpdateFrameWidth x ->
      let x' = fromFractional x :: Int
      in [Model $ model & frameWidth .~ x'
                        & animationState . ballPosition .~ Vector 1 1]

    UpdateFrameHeight x ->
      let x' = fromFractional x :: Int
      in [Model $ model & frameHeight .~ x'
                        & animationState . ballPosition .~ Vector 1 1]

    UpdateVelX x ->
      let x1 = fromFractional x :: Int
          x2 =  model ^. animationState . ballVelocity . getX
          
      in [ Model $ model & animationState . ballVelocity . getX
             .~ if x2 < 0 then negate x1 else x1 ]
    
    UpdateVelY y ->
      let y1 = fromFractional y :: Int
          y2 = model ^. animationState . ballVelocity . getY
          
      in [Model $ model & animationState . ballVelocity . getY
            .~ if y2 < 0 then negate y1 else y1 ]

    -- start animation
    AppStart -> [ Message "startLabel" AnimationStart ]

    FadeInRender -> [ Message "renderLabel" AnimationStart, Event RenderAnimation ]

    StartRendering -> [ Producer renderProducer ]

    -- disable start button after clicked
    DisableStartButton -> [ Model $ model & startBtnEnabled .~ False ]

    -- exit app
    AppExit -> [exitApplication]
 
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
  threadDelay 300000
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
