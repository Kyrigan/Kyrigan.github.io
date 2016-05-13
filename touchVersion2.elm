import Html exposing (..)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp
import Text exposing (..)
import List 
import Basics exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)

--How to play
--Navigate through the holes to answer the question correctly using the arrows
--The holes will not be discovered until you fall into them!


{-

Hackathon API Code Below: Change things in this section to make your game!!
------------------------------------------------------------------------------------
You can change things like the player's graphic, where the holes appear,
and the multiple choice question and answers.

-}


--Use this to change the player graphics!
player = group [  circle 10 |> filled black, 
                  circle 9 |> filled yellow
               ]

--Add new lines to this list to add new holes or change the current ones
--Format is (x positon, y position)
obsList = [
           (100,75)
          ,(90,-50)
          ,(110,10)
          ,(-100,-50)
          ,(-50,-40)
          ,(-90,30)
          ,(60,60)
          ]

--Change size of trap
--(1 is normal, more than 1 is bigger. Must be a whole number!)
trapSize = 1

--Change background colour of the play area
--List of possible colours here: http://package.elm-lang.org/packages/elm-lang/core/2.1.0/Color
backgroundColour = darkGreen

--Add visuals on top of the background
backgroundVisuals = group [  ]

initX = -200 --Change the starting x position of the player
initY = 0 -- Change the starting y-position of the player

--Change the question's text here
questionTxt = "(5+10)*5 = ?" 

--What are the possible options?
answerATxt = "75"
answerBTxt = "15"
answerCTxt = "20"
answerDTxt = "17"

--Which answer(s) is/are correct? (Use Correct or Wrong)
answerA = Correct
answerB = Wrong
answerC = Wrong
answerD = Wrong
               
               
--Change the look of the game end screen when you get the right answer
gameEndedScreenCorrect = group[filled black (rect 500 300), 
                              filled orange (rect 497 297),
                              filled black (rect 252 152), 
                              (filled (hsl 0.07 1 0.74)(rect 250 150)),
                              (Graphics.Collage.text (Text.bold (Text.fromString "Congratulations!"))) |> scale (2.2)
                              ]


--Change the look of the game end screen when you get the wrong answer
gameEndedScreenWrong = group[filled black (rect 500 300), 
                              filled red (rect 497 297),
                              filled black (rect 252 152), 
                              (filled (hsl 0.07 1 0.74)(rect 250 150)),
                              (Graphics.Collage.text (Text.bold (Text.fromString "GAME OVER!"))) |> scale (2.2)
                              ]

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------






--ADVANCED CODE BELOW, NOT FOR HACKATHON
--Don't change anything below here, may cause unexpected errors.






type Answer = Correct | Wrong

main = StartApp.start { model = model, view = view, update = update }

model = { x = initX, y = initY , areaW = areaWidth, areaH = areaHeight, onTrap = False, 
          obstacleList = obsList,
          visibleList = List.repeat len False, gameEndedCorrect = False, gameEndedWrong = False}
len = List.length obsList
view address model =   div [] [
                      table []
                [tr[] [
                      td [] [],
                      td [] [button [ onClick address Up ] [ Html.text "^" ]],
                      td [] []
                      ],
                 tr[] [
                      td [] [button [ onClick address Left ] [ Html.text "<" ]],
                      td [] [Html.text ("(" ++ toString model.x ++ "," ++ toString model.y ++ ")")],
                      td [] [ button [ onClick address Right ] [ Html.text ">" ]]
                      ],
                 tr[] [
                      td [] [],
                      td [] [button [ onClick address Down ] [ Html.text "v" ]],
                      td [] []
                      ]
                  ] ,
                      div [] [fromElement (shape model)]
                  
                      ]

shape model = collage (round (model.areaW)) (round (model.areaH)) 
                      [ rect (model.areaW) (model.areaH) |> filled black
                      , group [ rect (model.areaW-10) (model.areaH-10) |> filled backgroundColour, backgroundVisuals]
                      , question |> move(-185,110)
                      , obstacleDrawer model.obstacleList model.visibleList
                      , player
                      |> move (model.x,model.y) 
                      ,answerBG
                      ,answersTxt
                      ,if model.gameEndedCorrect then gameEndedScreenCorrect else group[]
                      ,if model.gameEndedWrong then gameEndedScreenWrong else group[]
                      ]
                      
answersTxt = group[ Graphics.Collage.text(Text.bold(Text.fromString answerATxt)) |> move (210,115) |> scale 2
                  , Graphics.Collage.text(Text.bold(Text.fromString answerBTxt)) |> move (210,37.5) |> scale 2
                  , Graphics.Collage.text(Text.bold(Text.fromString answerCTxt)) |> move (210,-35) |> scale 2
                  , Graphics.Collage.text(Text.bold(Text.fromString answerDTxt)) |> move (210,-110) |> scale 2]

areaWidth = 500 -- change the width of the play area 
areaHeight = 300 -- change the height of the play area

question = group [rect 102 52 |> filled black, rect 100 50 |> filled lightBlue, questionText]
questionText = group [Graphics.Collage.text (Text.bold(Text.fromString questionTxt))]

obstacleDrawer l lvis = group (List.map2 drawObstacle l lvis)
drawObstacle (x,y) vis = (obstacle vis) |> move (x, y)
obstacle vis = group [if vis then pothole else filled black (circle 0)]
obsradius = 700*trapSize
pothole = group [ (filled black (oval (trapSize*35) (trapSize*16))) |> scale 1.05
                 , (filled darkBrown (oval (trapSize*35) (trapSize*16)))
                 , (filled black (oval (trapSize*34) (trapSize*10))) |> scale 0.98 |> move (0,-(2.7*trapSize))]

answerBG= group[ rect 80 5          -- first option
                      |> filled black
                      |> move (210,-75)
                      , rect 80 5    --second option
                      |> filled black 
                      |> move (210,0)
                      , rect 80 5    --third option
                      |> filled black 
                      |> move (210,75)
               ]   

--Check given coordinate of trap comparison to model character location
onCoordinate : { a | x : number', y : number } -> (number,number) -> Bool

onCoordinate model (x,y) = func model (x,y) <= obsradius                           

trapChecker model = List.map (onCoordinate model) model.obstacleList

visUpdate model = List.map2 (onCoordinateVis model) model.obstacleList model.visibleList
onCoordinateVis model (x,y) visible = if func model (x,y) <= obsradius && visible == False then True 
                                   else if visible then True 
                                   else False

func model (x,y)= ((x-model.x)^2+(y-model.y)^2)
--Return True or false
listChecker : { a | x : number', y : number, obstacleList: List(number,number) } -> Bool
listChecker model = List.foldr (||) (False) (trapChecker model) 
--If empty, no true (no trap contact), thus return false else true
checkTrap model = (listChecker model) 

resetX = -150
resetY = 0

checkAnswerCorrect model = if model.x > 190 && model.y > 75 && answerA == Correct then True 
                    else if model.x > 190 && model.y > 0 && model.y < 75 && answerB == Correct then True
                    else if model.x > 190 && model.y > -75 && model.y < 0 && answerC == Correct then True
                    else if model.x > 190 && model.y < -75 && answerD == Correct then True
                    else False
                    
checkAnswerWrong model = if model.x > 190 && model.y > 75 && answerA == Wrong then True 
                    else if model.x > 190 && model.y > 0 && model.y < 75 && answerB == Wrong then True
                    else if model.x > 190 && model.y > -75 && model.y < 0 && answerC == Wrong then True
                    else if model.x > 190 && model.y < -75 && answerD == Wrong then True
                    else False

type Action = Up | Down | Left | Right

update action model =
  case action of
    Up -> { model | onTrap = checkTrap model,
                    visibleList = visUpdate model,
                    gameEndedCorrect = checkAnswerCorrect model,
                    gameEndedWrong = checkAnswerWrong model,
                    y = if not model.onTrap then model.y + 6 else resetY,
                    x = if model.onTrap then resetX else model.x}
    Down -> { model | onTrap = checkTrap model,
                      visibleList = visUpdate model,
                      gameEndedCorrect = checkAnswerCorrect model,
                      gameEndedWrong = checkAnswerWrong model,
                     y = if not model.onTrap then model.y - 6 else resetY,
                     x = if model.onTrap then resetX else model.x}
    Left -> { model | onTrap = checkTrap model,
                      visibleList = visUpdate model,
                      gameEndedCorrect = checkAnswerCorrect model,
                      gameEndedWrong = checkAnswerWrong model,
                      x = if not model.onTrap then model.x - 6 else resetX,
                      y = if model.onTrap then resetY else model.y}
    Right -> { model | onTrap = checkTrap model,
                       visibleList = visUpdate model,
                       gameEndedCorrect = checkAnswerCorrect model,
                       gameEndedWrong = checkAnswerWrong model,
                       x = if not model.onTrap then model.x + 6 else resetX,
                       y = if model.onTrap then resetY else model.y}