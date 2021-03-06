
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Text exposing (..)
import Window
import Touch exposing (..)


-- STUFF YOU GET TO CUSTOMIZE!

startScore = 0         -- Maybe you don't want to start with 0?

targetScore = 99 -- Set your target score right here, the number next to this message!

frameColor = black -- Changes the color of the frame!

speedofGame = 20 -- ARE YOU FAST OR ARE YOU FAST

startingTime = 300 -- This is measure in seconds.

character = -- Customize your character!
            group [ circle 10 |> filled red ] 
             
backdrop w h t win winTime
          = -- Here you can change what the background of the game is. 
                 -- 'w' is for width, 'h' is for height, 't' is for time!
                 group [rect w h
                            |> filled (rgb 100 100 100)
                            ,
                        gear 100 |> filled darkGray
                                 |> rotate (degrees t)
                        ,
                        gear 40 |> filled darkGray
                                 |> rotate (degrees -t)
                                 |> move (100,100)
                        ,
                        gear 40 |> filled darkGray
                                 |> rotate (degrees -t)
                                 |> move (-100,-100)
                        ,
                        circle 50 |> filled (rgb 100 100 100)
                        ,
                        circle 20 |> filled (rgb 100 100 100)
                                  |> move (100,100)
                        ,
                        circle 20 |> filled (rgb 100 100 100)
                                  |> move (-100,-100)
                        ,
                        plus darkGray |> scale 5
                                      |> rotate (degrees (-t/2))
                        ,
                        rect 10 30 |> filled darkGray
                                   |> rotate (degrees (t/2))
                                   |> move (100,100)
                        ,
                        rect 10 30 |> filled darkGray
                                   |> rotate (degrees (t/2))
                                   |> move (-100,-100)
                        ,
                        timerText t white 30 win winTime |> move (-300,320)
                        ]

gear size = polygon (List.map (getPoint size) [0..18])

getPoint size n = (size*cos(n),size*sin(n))

winBanner winPos t = -- Here you can change what the victory banner looks like.
                    group [gear 30 
                              |> filled black
                              |> move(200,-10)
                              |> rotate (degrees -t)
                           ,
                           circle 15
                              |> filled (rgb 100 100 100)
                              |> move(200,-10)
                              
                           ,
                           gear 30
                              |> filled black
                              |> move(-200,-10)
                              |> rotate (degrees t)
                           ,
                           circle 15
                              |> filled (rgb 100 100 100)
                              |> move(-200,-10)
                              
                           ,
                           text (Text.style (victoryStyle white) (fromString "VICTORY")) 
                           -- Make sure the victory text is at the bottom so it is on top of everything!

                           ]

loseBanner losePos t = -- Here you can change what the victory banner looks like.
                    group [gear 30 
                              |> filled black
                              |> move(200,-10)
                              |> rotate (degrees -t)
                           ,
                           circle 15
                              |> filled (rgb 100 100 100)
                              |> move(200,-10)
                              
                           ,
                           gear 30
                              |> filled black
                              |> move(-200,-10)
                              |> rotate (degrees t)
                           ,
                           circle 15
                              |> filled (rgb 100 100 100)
                              |> move(-200,-10)
                              
                           ,
                           text (Text.style (victoryStyle white) (fromString "TRY AGAIN")) 
                           -- Make sure the victory text is at the bottom so it is on top of everything!
                           ]
                           
                           
platformDesign = group -- Change what the platform looks like!
                    [ rect 100 10
                         |> filled black
                    ]

bannerPosition = 0 -- Changes the position of the banner. Make negative to go down, positive to go up.

-- First two numbers are x and y coordinates, stating where the platforms start at the
-- beginning of the game. The third number is the speed, positive to the right,
-- negative to the left.

platforms = [ ((200,60),1, UpDown),   ((200,270),1,   UpDown),   ((200,480),1,UpDown), 
              ((280,360),1,LeftRight), ((0,360),1,    LeftRight), ((-280,360),1,LeftRight), 
              ((280,120),1,LeftRight),  ((0,120),1,   LeftRight),  ((-280,120),1,LeftRight), 
              ((280,240),-1,LeftRight), ((0,240),-1,  LeftRight), ((-280,240),-1,LeftRight),
              ((-200,60),-1,UpDown),   ((-200,270),-1,UpDown),   ((-200,480),-1,UpDown)
                       ]




-- 'Add' -> Addition, 'Sub' -> Subtraction, 
-- 'Mul' -> Multiplication, 'Div' -> Subtraction

operations = [ (Add,8),(Sub,9),(Mul,0),
               (Mul,-1),(Sub,7),(Add,0),
               (Add,3),(Mul,1),(Add,6),
               (Sub,7),(Sub,5),(Div,2),
               (Add,4),(Add,8),(Sub,1),
               (Sub,10),(Sub,3),(Mul,2),
               (Add,10),(Sub,2),(Add,2)]

-- VERY IMPORTANT, Look up and make sure the number of operations matches
-- the number of platforms, or you just may run into a problem.

horizontalVelocity = 4 -- How fast you can move side to side
jumpPower = 7         -- How much leg day have you done?  
width = 425

-- Beyond here is all the messy code we came up with! ENTER AT YOUR OWN RISK !! 


-- 



--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--




--------------------------------------------------------------------------------------------- MODEL

type alias Model =
  { x : Float
  , y : Float
  , t : Float
  , groundHeight : Float
  , vx : Float
  , vy : Float
  , dir : Float
  , platformSpeed : Float
  , platformDir : Direction
  , oldvy : Float
  , platforms : List ((Float,Float),Float,Direction)
  , score : Float
  , operations : List (Operation,Int)
  , operated : Bool -- Ensures the operation on each platform can only occur once.
  , target : Float
  , win : Bool
  , lose : Bool
  , winPos : Float
  , winTime : Float
  , losePos : Float
  }


type Operation = Add | Sub | Mul | Div | Null



type Direction = UpDown | LeftRight  -- Vertical and Horizontal

type alias Keys = { x:Int, y:Int }


state : Model
state =
  { x = 0
  , y = 0
  , t = 0
  , groundHeight = 0
  , vx = 0
  , vy = 0
  , dir = 0 -- 0 is neutral, 1 is for upDown, 2 is for leftRight
  , platformSpeed = 0
  , platformDir = UpDown
  , oldvy = 0
  , platforms = platforms
  , score = startScore
  , operations = operations
  , operated = True
  , target = targetScore
  , win = False
  , lose = False
  , winPos = 0
  , winTime = startingTime
  , losePos = 0
  }


-- UPDATE

update : (Float, Keys, List Touch, (Int,Int)) -> Model -> Model
update (dt, keys, touches, (w,h)) state =
  state
    |> gravity dt
    |> updateTime dt -- For animations!
    |> jump keys touches h
    |> walk keys touches w
    |> physics dt h
    |> winning dt
    |> losing dt

updateTime dt state = { state | t = state.t + dt }  

winning dt state =  if state.lose then  { state | win = False} else
                    if state.win == False && state.score == state.target then
                      { state | win = True ,
                        winPos = state.winPos - dt,
                        winTime = startingTime - state.t/50 }
                   else if state.win == True && state.winPos > (-119 + bannerPosition)then
                   { state | 
                             winPos = state.winPos - dt }
                   else state

losing dt state =  if state.win then { state | lose = False} else if state.lose == False && state.t > (50*startingTime) then
                      { state | lose = True ,
                        losePos = state.losePos - dt }
                   else if state.lose == True && state.losePos > (-119 + bannerPosition)then
                   { state | 
                             losePos = state.losePos - dt }
                   else state

jump : Keys -> List Touch -> Int -> Model -> Model
jump keys touches h state =
  let touch = Maybe.withDefault { x = 0
                                , y = 0
                                , id = 69
                                , x0 = 0
                                , y0 = 0
                                , t0 = 0
                                }
              (List.head touches) -- Gets the top of the list.
  in if (keys.y > 0 || touchJump touch state h) && state.vy == 0 then
      { state | vy = jumpPower , oldvy = state.vy}
     else state


touchJump touch state h' =
  let h = toFloat h'
      y = toFloat touch.y
  in if touch.id == 69 then False
     else if (-y + h/2) > (state.y + 60 - h/2) then True
     else False



gravity : Float -> Model -> Model
gravity dt state = let platforms = state.platforms
                       groundHeight = List.maximum (List.map (landOnAPlatform state) platforms)
                       platformSpeed = List.maximum (List.map (speedCalc state) platforms)
                       dir = List.maximum (List.map (getDirection state) platforms)
                       change = List.maximum (List.map2 (updateScore state) platforms state.operations)
                   in if change == Just -9999 then
                            { state |
                                vy = if state.y > state.groundHeight || state.vy > 0 then state.vy - dt/4 else 0,
                                oldvy = state.vy,
                                dir = Maybe.withDefault 0 dir,
                                platformSpeed = if platformSpeed == Just -999 then 0 else Maybe.withDefault 0 platformSpeed,
                                operated = if state.operated == True && state.vy < 0 then False else state.operated,
                                 groundHeight = Maybe.withDefault 0 groundHeight
                            }
  
                      else  { state |
                              vy = if state.y > state.groundHeight || state.vy > 0 then state.vy - dt/4 else 0,
                              oldvy = state.vy,
                              dir = Maybe.withDefault 0 dir,
                              platformSpeed = if platformSpeed == Just -999 then 0 else Maybe.withDefault 0 platformSpeed,
                              operated = True,
                              groundHeight = Maybe.withDefault 0 groundHeight,
                              score = Maybe.withDefault 0 change
                          
                        }



landOnAPlatform state ((xPlat,yPlat),vxPlat,dir)
  = if nearEnough (xPlat,yPlat) (state.x,state.y)
      then yPlat + 15

      else 0

speedCalc state ((xPlat,yPlat),vxPlat,dir)
  = 
  
     if nearEnough (xPlat,yPlat) (state.x,state.y) && (state.oldvy == 0 || state.vy ==0)
      then vxPlat

      else -999

getDirection state ((xPlat,yPlat),vxPlat,dir)
  = 
  
     if nearEnough (xPlat,yPlat) (state.x,state.y) 
      then if dir == LeftRight then 2 else 1

      else -999


updateScore state ((xPlat,yPlat),vxPlat,dir) (op,z)
  = if (state.oldvy == 0 && nearEnough (xPlat,yPlat) (state.x,state.y) && state.operated == False )

      then case op of
            Add -> state.score + toFloat z
            Sub -> state.score - toFloat z
            Mul -> state.score * toFloat z
            Div -> state.score / toFloat z
            Null -> state.score
      else -9999
      
nearEnough (x1,y1) (x2,y2) = abs(x1-x2) < 55 && (y2-y1) < 38 && (y2-y1) > 5

nearEnough2 (x1,y1) (x2,y2) = abs(x1-x2) < 55 && (y2-y1) < 100 && (y2-y1) > -100

physics : Float -> Int -> Model -> Model
physics dt h state  =
  { state |
      x = if state.x < -(width-50) then (width-50)
          else if state.x > (width-50) then -(width-50) else
            if state.dir == 2 then
            state.x + dt * (state.vx+state.platformSpeed) else state.x + dt * state.vx,
      y = if state.dir == 1 then 
            if state.platformSpeed < 0 then
                  min state.groundHeight (state.y + dt * state.vy)
             else max state.groundHeight (state.y + dt * (state.vy))
          else
             max state.groundHeight (state.y + dt * state.vy)
      , platforms = List.map (platformPhysics dt h) state.platforms
  }

platformPhysics dt h ((x,y),vx,dir) = let height = toFloat h
                                      in if dir == LeftRight then
                                         if x > width
                                         then ((-width,y),vx,dir)
                                         else if x < -width then ((width,y),vx,dir)
                                                            else ((x+vx*dt,y),vx,dir)
                                         else if y > height
                                              then ((x,-30),vx,dir)
                                              else if y < -30 then ((x,height),vx,dir)
                                                            else ((x,y+vx*dt),vx,dir)

walk : Keys -> List Touch -> Int -> Model -> Model
walk keys touches w state = 
  let touch = Maybe.withDefault { x = 0
                                , y = 0
                                , id = 69
                                , x0 = 0
                                , y0 = 0
                                , t0 = 0
                                }
              (List.head touches) -- Gets the top of the list.
  in { state |
      vx = (toFloat keys.x + (touchMove touch state w))* horizontalVelocity
      }


touchMove touch state w' =
  let w = toFloat w'
      x = toFloat touch.x
  in if touch.id == 69 then 0 
     else if (x-w/2) > (state.x+horizontalVelocity) then 1
     else if (x-w/2) < (state.x-horizontalVelocity) then -1
     else 0



-- VIEW

view : (Int, Int) -> Model -> Element
view (w',h') state =
  let
    (w,h) = (toFloat w', toFloat h')


    groundY = 60 - h/2

    position =
      (state.x, state.y + groundY)
  in
    collage w' h' <|
      [ backdrop w h state.t state.win state.winTime
      
      , character
          |> move position
      ]  ++
                (List.map2 (platform groundY) state.platforms state.operations)
         ++
      [
       rect w 50
          |> filled frameColor
          |> move (0, 24 - h/2)
      ,
       rect w h
          |> filled frameColor
          |> moveX (-w/2 - width + 50)
       ,
       rect w h
          |> filled frameColor
          |> moveX (w/2 + width - 50)
       , 
       text (Text.concat [Text.style (scoreStyle white) (fromString ("Do the math! ")),
                          Text.style (scoreStyle red) (fromString (if state.win then toString state.target else toString state.score)),
                          Text.style (scoreStyle white) (fromString (" = " ++ toString state.target))
                         
                         ] )
           |> move (0, 28 - h/2)
           
       , winBanner state.winPos state.t
          |> move (0, 60 + h/2 + state.winPos)
       , loseBanner state.losePos state.t
          |> move (0, 60 + h/2 + state.losePos)
      ]
               
platform groundY ((x,y),vx,dir) (op,z) = group [ platformDesign
                                            , operator op z |> moveY -5
                                             ] |> move (x,y+groundY)

operator op z = let pos = if z >= 10 || z < 0 then -8 else -6
    
                in case op of
                Add -> group [ filled white (rect 40 20) ,
                               plus red |> moveX pos, 
                               text (Text.style (opStyle red) (fromString (toString z)))
                               |> move(8,2)
                               ]
                Sub -> group [ filled white (rect 40 20)  ,
                               filled blue (rect 12 4) |> moveX pos,
                               text (Text.style (opStyle blue) (fromString (toString z)))
                               |> move(8,2)
                               ]
                Mul -> group [ filled white (rect 40 20) ,
                               plus darkGreen |> rotate (degrees 45)
                                              |> moveX pos,
                               text (Text.style (opStyle darkGreen) (fromString (toString z)))
                               |> move(8,2)]
                Div -> group [ filled white (rect 40 20),
                               filled purple (rect 12 4) |> moveX pos,
                               filled purple (circle 2) |> move (pos,5),
                               filled purple (circle 2) |> move (pos,-5),
                               text (Text.style (opStyle purple) (fromString (toString z)))
                               |> move(8,2)]
                Null -> group [ filled darkGrey (rect 40 20) ]

timerText t color size win winTime = text( Text.style (timerStyle color size) 
                                    (fromString ("Time: " ++ toString 
                                      ( if win then ceiling(winTime) else
                                        if ceiling (startingTime - t/50) < 0 then 0 else ceiling (startingTime - t/50)))) )
                                    

timerStyle color size ={ typeface = [ "Arial" ]
                  , height   = Just size
                  , color    = color
                  , bold     = True
                  , italic   = False
                  , line     = Nothing
                  }

-- STYLES AND SHAPES

opStyle color ={ typeface = [ "Arial" ]
               , height   = Just 16
               , color    = color
               , bold     = True
               , italic   = False
               , line     = Nothing
               }
               
               
victoryStyle color ={ typeface = [ "Arial" ]
               , height   = Just 60
               , color    = color
               , bold     = True
               , italic   = False
               , line     = Nothing
               }

scoreStyle color ={ typeface = [ "Arial" ]
               , height   = Just 24
               , color    = color
               , bold     = True
               , italic   = False
               , line     = Nothing
               }

heart color size = group [circle (size/2)
                            |> filled color
                            |> moveY (size/2),
                          circle (size/2)
                            |> filled color
                            |> moveX (size/2),
                          square size
                            |> filled color ]

plus color = group [ filled color (rect 4 12),
                     filled color (rect 12 4) ]


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update state input)


input : Signal (Float, Keys, List Touch, (Int,Int))
input =
  let
    delta = Signal.map (\t -> t/speedofGame) (fps 30)
  in
    Signal.sampleOn delta (Signal.map4 quadTuple delta Keyboard.arrows Touch.touches Window.dimensions)

quadTuple a b c d = (a,b,c,d)