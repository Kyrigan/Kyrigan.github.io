import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Keyboard
import Signal
import List exposing (map)
import Array exposing (Array)
import Text exposing (..)
import Window exposing (..)

-- FUNCTIONS -------------------
mod x n = let y = toFloat (floor (x / n)) -- This function is how I make things loop!
          in x - y * n

trans t y = if t < 0 -- Used for all permanent transitions (fading out, color change, etc.)
               then 0 
            else if (t/500) > (pi/2) then y
            else sin(t/500) * y

drawLine t (x1,y1) (x2,y2) = let distanceX = x2-x1
                                 distanceY = y2-y1
                                 newX = x1 + (trans t distanceX)
                                 newY = y1 + (trans t distanceY)
                             in  if (x1,y1) == (newX,newY) then
                                         segment (4000,4000) (4000,4000) 
                                 else
                                         segment (x1,y1) (newX,newY)
--------------------------------

--   STYLES  -------------------
yellowStyle = { typeface = ["Helvetica"]
 , height = Just 100.0
 , color = yellow
 , bold = True
 , italic = False
 , line = Nothing
 }
 
blueTitle = { typeface = ["Helvetica"]
 , height = Just 100.0
 , color = rgb 43 43 250
 , bold = True
 , italic = False
 , line = Nothing
 }

redTitle = { typeface = ["Helvetica"]
 , height = Just 100.0
 , color = rgb 250 34 34
 , bold = True
 , italic = False
 , line = Nothing
 }

greenTitle = { typeface = ["Helvetica"]
 , height = Just 100.0
 , color = rgb 34 250 34
 , bold = True
 , italic = False
 , line = Nothing
 }
 
blueTitleFade t = { typeface = ["Helvetica"]
 , height = Just 100.0
 , color = rgba 43 43 250 (trans (t-1500) 1)
 , bold = True
 , italic = False
 , line = Nothing
 }

blueTitleFadeOut t = { typeface = ["Helvetica"]
 , height = Just 100.0
 , color = rgba 43 43 250 (1-(trans (t-1500) 1))
 , bold = True
 , italic = False
 , line = Nothing
 }

redTitleFade t = { typeface = ["Helvetica"]
 , height = Just 100.0
 , color = rgba 250 34 34 (trans (t-1500) 1)
 , bold = True
 , italic = False
 , line = Nothing
 }

greenTitleFade t = { typeface = ["Helvetica"]
 , height = Just 100.0
 , color = rgba 34 250 34 (trans (t-1500) 1)
 , bold = True
 , italic = False
 , line = Nothing
 }
 
axisStyle = { typeface = ["Helvetica"]
 , height = Just 40.0
 , color = blue
 , bold = True
 , italic = False
 , line = Nothing
 }

footStyle = { typeface = ["Helvetica"]
 , height = Just 20.0
 , color = blue
 , bold = False
 , italic = False
 , line = Nothing}
 
blueTitle2 = { typeface = ["Helvetica"]
 , height = Just 60.0
 , color = blue
 , bold = True
 , italic = False
 , line = Nothing
 }

blueFoot = { typeface = ["Helvetica"]
 , height = Just 30.0
 , color = blue
 , bold = False
 , italic = False
 , line = Nothing
 } 

blueTextFade t = { typeface = ["Helvetica"]
 , height = Just 30.0
 , color = rgba 52 101 164 (trans t 1)
 , bold = False
 , italic = False
 , line = Nothing
 } 

blueFoot2 = { typeface = ["Helvetica"]
 , height = Just 15.0
 , color = blue
 , bold = False
 , italic = False
 , line = Nothing
 }

titleStyle3 = { typeface = ["Helvetica"]
 , height = Just 60.0
 , color = blue
 , bold = True
 , italic = False
 , line = Nothing
 }

factStyle = { typeface = ["Helvetica"]
 , height = Just 30.0
 , color = blue
 , bold = False
 , italic = False
 , line = Nothing
 }  

borderBlue = { color = blue
    , width = 3.0
    , cap = Round
    , join = Smooth
    , dashing = []
    , dashOffset = 0
    } 

thickRed = { color = red
    , width = 10.0
    , cap = Round
    , join = Smooth
    , dashing = []
    , dashOffset = 0
    }

thickGreen = { color = green
    , width = 10.0
    , cap = Round
    , join = Smooth
    , dashing = []
    , dashOffset = 0
    }

titleStyle4 t = { typeface = ["hi"]
 , height = Just 35
 , color = rgb 0 0 0
 , bold = True
 , italic = False
 , line = Nothing
 }

captionStyle = { typeface = ["hi"]
 , height = Just 30.0
 , color = rgb 0 0 0
 , bold = True
 , italic = False
 , line = Nothing
 }

orangeOutline = {
    color = orange
    , width = 1.5
    , cap = Round
    , join = Smooth
    , dashing = []
    , dashOffset = 0
    } 
 
--------------------------------


-- Code used to create the slides (Not mine credit goes to other group.)

type Events = Arrows {x : Int, y:Int} 
            | TimeIncrease Float

events : Signal Events
events =
   Signal.merge
       (Signal.map Arrows       Keyboard.arrows)
       (Signal.map TimeIncrease (fps 64))

main =
 Signal.map2 view Window.dimensions <| Signal.foldp update (0,0) events

update event (t,idx)
 = case event of
     Arrows arrows   -> (0     , moveIdx arrows idx)
     TimeIncrease dt -> (t + 1 * dt, idx)

moveIdx arrows oldIdx = case arrows.x of
                        1  -> if oldIdx + 1 < Array.length slides 
                              then oldIdx + 1
                              else oldIdx
                        (-1) -> if oldIdx > 0
                              then oldIdx - 1
                              else oldIdx
                        otherwise -> oldIdx

view (w,h) (t,idx) = case Array.get idx slides of
                Just slide -> slide t (w,h)
                Nothing    -> collage 400 400 []

slides = Array.fromList [slide1,slide2,slide3,slide4,slide5,slide6,slide7,slide8,slide9,slide10,slide11]

--------------------------------------------------------------------------------------------------------

-- SLIDES ARE BELOW --

slide1 t' (w,h)= let t = mod t' 11000 in collage w h [ 
                      line (t/3),
                      pacman t |> scale (if t<7000 then 1+(t/4000)^10 else 10),
                      text (Text.color white (fromString "BIG"))
                        |> move (-519,20) |> scale (15)
                      ,
                      text (Text.color white (fromString "DATA"))
                        |> move (-430,-130) |> scale (15)
                      ,
                      text (Text.color white (fromString "By Ray and Kristie"))
                        |> move (-460,-250) |> scale (4)
                      ,
                      text (Text.color white (fromString "(Use the left and right arrow keys to navigate the presentation.)"))
                        |> move (-325,-300) |> scale (2)
                      ]

pacman t = group [semicircle
                            |> filled yellow 
                            |> rotate (degrees (((abs(cos(t/150))))* 45))
                  ,
                  semicircle
                            |> filled yellow 
                            |> rotate (degrees (180 - ((abs(cos(t/150))))* 45))
                  ]

semicircle = polygon (List.map getpoint [0..60])

getpoint n = (cos(degrees (3*n))*100,sin(degrees (3*n))*100)

line t = group 
    [toForm (image 70 70 "tweet.png")
      |> move (disappear(790 - t), 0) 
      ,
    toForm (image 60 45 "youtube.png")
      |> move (disappear(900 - t), 0) 
      ,
    toForm (image 70 70 "linkin.png")
      |> move (disappear(1010 - t), 0) 
      ,
    toForm (image 80 80 "google.png")
      |> move (disappear(1120 - t), 0) 
      ,
    toForm (image 80 80 "amazon.png")
      |> move (disappear(1230 - t), 0)
      ,
    toForm (image 70 70 "facebook.png")
      |> move (disappear(1340 - t), 0)
      ,
    toForm (image 70 70 "flickr.png")
      |> move (disappear(1450 - t), 0)
      ,
    toForm (image 70 70 "instagram.png")
      |> move (disappear(1560 - t), 0)
      ,
    toForm (image 70 70 "times.png")
      |> move (disappear(1670 - t), 0)
      ,
    toForm (image 70 70 "data.png")
      |> move (disappear(1780 - t), 0)
    ]

disappear x = if x < -30 then 3000 else x
-----------------------------------------
slide2 t (width,height) = collage width height [ poptartWheel (t/400), poptartWheel (-t/400) |> scale (1.5),  poptartWheel (t/400) |> scale (2),
                      man t |> moveY (-30) |> moveX (getX (t/5))
                      ,
                       text (style (redTitleFade (t)) (fromString "= Obesity"))
                        |> move (-100 + (trans (t-1500) 100),-100)
                      ,
                       text (style (redTitleFade (t+1000)) (fromString "Poptarts + Human"))
                        |> move (0,120 + (trans t 100)) ]

getX t = let
                startLeft = -150
                startRight = 150
               in
                case (round(t)//300)%2 == 0 of
                True -> startLeft + (toFloat(round(t)%(300)))
                False -> startRight - (toFloat(round(t)%(300)))

man t = group
       [ belly t,
         circle 40
            |> filled black
            |> move (0,150)
         ,
         rect 20 80
           |> filled black
           |> move (-15,(0 + 20* abs(sin(t/200))))
         ,
         rect 20 80
           |> filled black
           |> move (15,(0 + 20* abs(sin(t/200+pi/2))))
         , 
         mouth t
         , 
         armsA t
       ]
  
mouth t= let timer = cos (t/500) in if timer > 0 then 
          polygon [(0,270),(80,(((abs(cos(t/150))))*70)+240),(80,(((abs(cos(t/150))))*(-70))+300)]
          |> filled white
          |> scale 0.5 
          else
          polygon [(0,270),(-80,(((abs(cos(t/150))))*70)+240),(-80,(((abs(cos(t/150))))*(-70))+300)]
          |> filled white
          |> scale 0.5 
   
poptartWheel t = group [poptart 0 t,poptart 1 t,poptart 2 t,poptart 3 t,poptart 4 t,poptart 5 t,
                        poptart 6 t,poptart 7 t,poptart 8 t,poptart 9 t,poptart 10 t,poptart 11 t,
                        poptart 12 t,poptart 13 t,poptart 14 t,poptart 15 t,poptart 16 t,poptart 17 t]

poptart n t = let angle = degrees (20 * ((toFloat n)+t))
              in  toForm (image 60 45 "poptart.png") |> move (330 * cos angle, 330 * sin angle)
      
armsA t = group
  [      rect 80 20
           |> filled black
           |> move ((if t<9000 then (-0.5)*(20+(t/60)) else -85), 80)
           |> rotate (-0.5 + sin (t/80))
      ,
         rect 80 20
           |> filled black
           |> move ((if t<9000 then (0.5)*(20+(t/60)) else 85), 80)
           |> rotate (0.5 + sin (t/80))
  ]
  
belly t = oval (if t<9000 then 60+(t/60) else 210) (if t<1000 then (95+(t/60)) else 110)
            |> filled black
            |> move (0,65)
-----------------------------------------
slide3 t (w,h) =
   collage w h [ 
   text (style (redTitle) (fromString "V")) |> move (-250,(trans (t-1500) 120)),
   text (style (blueTitle) (fromString "V")) |> move (-250,0),
   text (style (blueTitleFadeOut t) (fromString "There are 3")) |> move (-100,100),
   text (style (blueTitleFadeOut t) (fromString "s of Big Data.")) |> move (100,0),
   text (style (greenTitle) (fromString "V")) |> move (-250,(trans (t-1500) -120)),
   text (style (redTitleFade t) (fromString "OLUME")) |> move (-40,(trans (t-1500) 120)),
   text (style (blueTitleFade t) (fromString "ELOCITY")) |> move (0,0),
   text (style (greenTitleFade t) (fromString "ARIETY")) |> move (-45,(trans (t-1500) -120)),
   text (style (blueFoot2)(fromString 
   "Source: blogs.gartner.com/doug-laney/files/2012/01/ad949-3D-Data-Management-Controlling-Data-Volume-Velocity-and-Variety.pdf"))
   |> move (0,-250)]
--------------------
slide4 t (w,h) = 
  collage w h [ text (style blueTitle2 (fromString ("DATA VOLUME"))) |> move (0,280),
  bar (trans t 2) |> moveX(-180),
                     bar (trans (t-50) 8) |> moveX (-153),
                     bar (trans (t-100) 18) |> moveX (-126),
                     bar (trans (t-150) 32) |> moveX (-99),
                     bar (trans (t-200) 50) |> moveX (-72),
                     bar (trans (t-250) 72) |> moveX (-45),
                     bar (trans (t-300) 98) |> moveX (-18),
                     bar (trans (t-350) 128) |> moveX (9),
                     bar (trans (t-400) 162) |> moveX (36),
                     bar (trans (t-450) 200) |> moveX (63),
                     bar (trans (t-500) 242) |> moveX (90),
                     bar (trans (t-550) 288) |> moveX (117),
                     bar (trans (t-600) 338) |> moveX (144),
                     bar (trans (t-650) 392) |> moveX (171),
                     text (style (blueTextFade (t-1000)) (fromString "90% of all data in the world")) |> move (-483,160),
                     text (style (blueTextFade (t-1000)) (fromString "was collected in the last 2 years.")) |> move (-445,120),
                     text (style axisStyle (fromString "Time")) |> move (0,-220),
                     text (style footStyle (fromString "1970")) |> move (-180,-210),
                     text (style footStyle (fromString "2015")) |> move (170,-210),
                     text (style footStyle (fromString "Warning: Diagram not to scale.")) |> move (0,-250),
                     text (style axisStyle (fromString "Volume")) |> move (-240,0) |> rotate (degrees 90),
                     outlined (solid blue) (segment (-210,-200) (210,-200)),
                     filled blue (ngon 3 10) |> move (210,-200) |> rotate (degrees 0),
                     filled blue (ngon 3 10) |> move (-210,200) |> rotate (degrees 90),
                     outlined (solid blue) (segment (-210,-200) (-210,200)),
                     text (style blueFoot2 (fromString ("Source: www.sintef.no/en/corporate-news/big-data--for-better-or-worse (2013)"))) |> move (0,-290)]

bar x = group [filled blue (rect 20 x) |> moveY (x/2-200)]
----------------------------------------------------------
slide5 t' (width,height) = 
 let 
  a = if t < 2800 then 1 else (t - 2800)*6
  t = mod t' 7500
  moveTweet (tOff,tScale,amp) = tweet |> move ((t-tOff)/2- toFloat(width),sin(t/tScale)*amp)
  shiftLeft (a,b,c) = (a+2200,b,c)
 in
  collage width height [ text (style blueTitle2 (fromString ("DATA VELOCITY"))) |> move (0,280),
  text (style blueFoot (fromString ("Twitter averages at 6000 tweets per second. (Diagram still not to scale.)"))) |> move (0,220),
  text (style blueFoot (fromString ("Incoming Tweets: " ++ (toString a)))) |> move (0,-220),
  text (style blueFoot2 (fromString ("Source: onesecond.designly.com"))) |> move (0,-250),
  tweet |> move ((t+1000)/2-toFloat(width),sin(t/200)*100),
  group <| List.map moveTweet [(1400,200,110),(1500,250,120),(1600,180,150),(1700,230,100),(1800,200,140),
  (1900,200,80),(2000,250,120),(2100,180,130),(2200,230,90),(2300,200,100),(2400,250,120),(2500,180,150),(2600,230,100),
  (2700,200,140),(2800,200,80),(2900,250,120),(3000,180,130),(3100,230,90),(3200,200,100),(1450,-200,115),(1550,-250,125),
  (1650,-180,155),(1750,-230,105),(1850,-200,140),(1950,-200,85),(2050,-250,125),(2150,-180,135),(2250,-230,95),(2350,-200,105),
  (2450,-250,125),(2550,-180,150),(2650,-230,100),(2750,-200,145),(2850,-200,90),(2950,-250,125),(3050,-180,135),(3150,-230,95),(3250,-200,105)]]

tweet = toForm (image 70 70 "tweet.png")
----------------------------------------
slide6 t' (width,height) = let t = mod t' 6500 in collage width height [
 text (style titleStyle3 (fromString ("DATA VARIETY"))) 
      |> move (0,280),
 text (style (blueTextFade (t-500)) (fromString "Over the course of 9 months,")) 
      |> move (-422 + (trans (t-500) 100),100),
 text (style (blueTextFade (t-500)) (fromString "a team at MIT collected data like")) 
      |> move (-400 + (trans (t-500) 100),60),
 text (style (blueTextFade (t-500)) (fromString "geo-location and device usage")) 
      |> move (-413 + (trans (t-500) 100),20),
 text (style (blueTextFade (t-500)) (fromString "from 100 mobile phones")) 
      |> move (-455 + (trans (t-500) 100),-20),
 text (style (blueTextFade (t-500)) (fromString "to create a dataset on human behaviour.")) 
      |> move (-347 + (trans (t-500) 100),-60),
 text (style blueFoot2 (fromString ("Source: realitycommons.media.mit.edu/pdfs/realitymining.pdf"))) 
      |> move (0,-250),
 phone 
      |> move (200,0) 
      |> rotate (vibrate (t/1000)),
 instagram 
      |> scale (getSize (t/1000)) 
      |> move (200,0), 
 gmap 
      |> scale (getSize ((t/1000)+pi/2)) 
      |> move (210,10),
 message 
      |> scale (getSize ((t/1000)+pi)) 
      |> move (200,0),
 facebook 
      |> scale (getSize ((t/1000)+3*pi/2)) 
      |> move (200,0)]

vibrate t = let vibration = degrees (cos(t*22)*5) in
            if t > 5 then vibration else 0
 
getSize t = 
  let size = cos (t) * 4 - 2.7
  in if size < 0 then 0 else if size > 1 then 1 else size 

phone = toForm (image 400 400 "phone.png")

instagram = toForm (image 140 140 "instagram.png")

gmap = toForm (image 150 150 "gmap.png")

facebook = toForm (image 140 140 "facebook.png")

message = toForm (image 150 150 "message.png")
----------------------------------------------
slide7 t' (width,height) = let t = mod t' 5000 in collage width height [   person (colorTrans (t-2300)) |> move (50,80),
  person (colorTrans (t-2300)) |> move (-50,80),
  person (colorTrans (t-2300)) |> move (50,-180),
  person (colorTrans (t-2300)) |> move (-50,-180),
  person (colorTrans (t-500)) |> move (0,-50),
  person (colorTrans (t-2300)) |> move (100,-50),
  person (colorTrans (t-2300)) |> move (-100,-50),
  magnify |> move (125,-100) |> scale (1+(trans (t-2000) 1)) |> move ((trans (t-2000) 110),(trans (t-2000) -34)),
  text (style titleStyle3(fromString ("MORE"))) |> move (0,280),
  text (style blueFoot(fromString ("Sampling is a relic of the past, now we can use all of a data set."))) |> move (0,215)]

magnify = toForm (image 600 450 "magnify.png")

colorTrans t = rgb (80+round(trans t 120)) (80+round(trans t 60)) (round(245-(trans t 245)))

person c = group [group [ filled c (circle 45) |> moveY (130),
                 filled c (rect 100 150),
                 filled c (rect 45 150) |> move (-27.5,-149),
                 filled c (rect 45 150) |> move (27.5,-149),
                 filled c (circle 22.5) |> move (-27.5,-224),
                 filled c (circle 22.5) |> move (27.5,-224),
                 arms c |> moveY (-10)
                 ] |> scale (0.4)]


arms c = group [filled c (rect 45 130) |> move (-75,0),
                 filled c (rect 45 130) |> move (75,0),
                 filled c (circle 22.5) |> move (-75,-65),
                 filled c (circle 22.5) |> move (75,-65),
                 filled c (circle 22.5) |> move (-75,65),
                 filled c (circle 22.5) |> move (75,65)]
----------------------------------------------------------
slide8 t' (width,height) = let t = mod t' 8000 in collage width height [ plot |> moveY (-50), dots (t-500) |> moveY (-50),
  outlined borderBlue (drawLine (t-3600) (-180,-81) (180,4)),
  outlined (dotted blue) (drawLine (t-4200) (-5,-5) (3,-36)),
  outlined (dotted red) (drawLine (t-4200) (-8,-75) (-16,-44)),
  outlined (dotted blue) (drawLine (t-3000) (-180,-46) (180,39)),
  outlined (dotted red) (drawLine (t-3000) (-180,-116) (180,-31)),
  text (style titleStyle3(fromString ("MESSINESS"))) |> move (0,280),
  text (style blueFoot(fromString ("Trading off exactitude for looking at all the data."))) |> move (0,215)]

plot =  group [      outlined (solid blue) (segment (-210,-200) (210,-200)),
                     filled blue (ngon 3 10) |> move (210,-200) |> rotate (degrees 0),
                     filled blue (ngon 3 10) |> move (-210,200) |> rotate (degrees 90),
                     outlined (solid blue) (segment (-210,-200) (-210,200)),
                     text (style footStyle (fromString ("x1"))) |> move (208,-213),
                     text (style footStyle (fromString ("x2"))) |> move (-232,200)]

dots t = let dotb tOff = filled (rgba 87 87 245 (trans (t-tOff) 1)) (circle 8)
             dotr tOff = filled (rgba 245 87 87 (trans (t-tOff) 1)) (circle 8)
         in group[dotb 0    |> move (-5,45),
                  dotb 100  |> move (-40,65), 
                  dotb 200  |> move (-70,30), 
                  dotb 300  |> move (-115,50), 
                  dotb 400  |> move (-110,110), 
                  dotb 500  |> move (-58,122), 
                  dotb 600  |> move (-80,80), 
                  dotb 700  |> move (-10,130), 
                  dotb 800  |> move (21,80), 
                  dotb 900  |> move (-15,95), 
                  dotb 1000 |> move (30,100), 
                  dotb 1100 |> move (-144,78), 
                  dotr 1200 |> move (120,-23), 
                  dotr 1300 |> move (60,-49), 
                  dotr 1400 |> move (80,-76), 
                  dotr 1500 |> move (10,-92), 
                  dotr 1600 |> move (5,-50), 
                  dotr 1700 |> move (-30,-70), 
                  dotr 1800 |> move (-50,-120), 
                  dotr 1900 |> move (-90,-80), 
                  dotr 2000 |> move (-8,-25), 
                  dotr 2100 |> move (42,-70), 
                  dotr 2200 |> move (63,-8), 
                  dotr 2300 |> move (32,-100)]
----------------------------------
slide9 t' (width,height) = let t = mod t' 6000 in collage width height [
 text (style titleStyle3(fromString ("CORRELATION"))) |> move (0,280),
 text (style blueFoot(fromString ("Moving away from causation in favor of looking for patterns and correlation."))) |> move (0,215),
 outlined borderBlue (square 80) |> move (-170, 60),
 outlined borderBlue (square 80) |> move (-170, -60),
 outlined thickRed (drawLine (t-1000) (-200,90) (-140,30)),
 outlined thickRed (drawLine (t-1500) (-200,30) (-140,90)),
 outlined thickGreen (drawLine (t-2500) (-200,-75) (-185,-90)),
 outlined thickGreen (drawLine (t-3000) (-185,-90) (-140,-30)),
 outlined borderBlue (drawLine (t-1000) (150,120) (150,-120)),
 outlined borderBlue (drawLine (t-1700) (150,-120) (410,-120)),
 outlined borderBlue (drawLine (t-2400) (150,-60) (210,-25)),
 outlined borderBlue (drawLine (t-3100) (210,-25) (270,-45)),
 outlined borderBlue (drawLine (t-3800) (270,-45) (330,0)),
 outlined borderBlue (drawLine (t-4500) (330,0) (390,110)),
 text (style titleStyle3 (fromString ("WHY"))) |> move (-20,70),
 text (style titleStyle3 (fromString ("WHAT"))) |> move (0,-50)]
----------------------------------------------------------------
slide10 t' (width,height) = let t = mod t' 6000 in
  collage width height [pacman t |> moveX (t - toFloat(width)/2 - 2500),
                      text (style (yellowStyle) (fromString "LOL")) |> moveX (t - toFloat(width)/2 - 2750),   
                      rect 5000 3
                        |> filled black
                        |> move (0, -140)
                      ,
                      sign t
                        |> move (-60, -120)
                        |> rotate 0.1
                      ,
                      lightbulbs t
                      ,
                      lightbulbs t
                        |> move (7, -70)
                      ,
                      text (style (titleStyle4 t) (fromString "DANGER")) |> move (-60,-115) |> rotate 0.1
                      ,
                      outlinedText orangeOutline (style (titleStyle4 t) (fromString "DANGER")) |> move (-60,-115) |> rotate 0.1
                      ,
                      fallingMan t
                        |> move (0, -(t/4) + toFloat(height)/2 + 100 )
                        |> rotate (20*sin (t/50000))
                      ,
                      square 200
                        |> filled white
                        |> move (0,-315)
                      ,
                       text (style (captionStyle) (fromString "Big Data")) |> move (-10, -230)
                      ,
                      oval 300 40
                        |> filled black
                        |> move (0,-200)
                      ]

fallingMan t = group
               [
                 circle 30
                   |> filled black
                   |> move (20,-40)
                 ,
                 rect 20 100
                    |> filled black
                    |> rotate 0.3
                 ,
                 rect 10 50
                    |> filled black
                    |> rotate ((1/10)*sin (t/100) - 0.5)
                    |> move (-15,-20)
                 ,
                 rect 10 50
                    |> filled black
                    |> rotate ((1/10)*sin (t/100) + 1.3)
                    |> move (27,-2)
                 ,
                 rect 15 25
                    |> filled black
                    |> rotate -0.5
                    |> move (0,50)
                 ,
                 rect 15 25
                    |> filled black
                    |> rotate 0.2
                    |> move (-17,50)
                 ,
                 rect 15 30
                    |> filled black
                    |> rotate 0.7
                    |> move (-27,70)
                 ,
                 rect 15 30
                    |> filled black
                    |> rotate 0.2
                    |> move (2,70)
                ]
sign t =group
        [
          rect 10 30
            |> filled grey
            |> move (-60, -55)
          ,
          rect 10 30
            |> filled grey
            |> move (60, -55)
          ,
          rect 200 80
            |> filled lighter
          ,
          rect 180 60
            |> filled white
        
        ]
lighter = rgba 255 153 0 0.6

lightbulbs t = group
             [
               circle 3
                 |> filled (yellowPulse t)
                 |> move (28,-76)
               ,
               circle 3
                 |> filled (yellowPulse t)
                 |> move (-2,-79)
               ,
               circle 3
                 |> filled (yellowPulse t)
                 |> move (-32,-82)
               ,
               circle 3
                 |> filled (yellowPulse t)
                 |> move (-62,-85)
               ,
               circle 3
                 |> filled (yellowPulse t)
                 |> move (-92,-88)
               ,
               circle 3
                 |> filled (yellowPulse t)
                 |> move (-122,-91)
               ,
               circle 3
                 |> filled (yellowPulse t)
                 |> move (-152,-94)                
             ]
                 
yellowPulse t = rgb 255 255 ((round((abs(cos (t/300)))*150))+100)
-----------------------------------------------------------------
slide11 t' (width,height) = collage width height [toForm (image 1200 586 "satire.jpg")]