import Graphicsvg exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Time exposing (Time, millisecond)
import Http exposing (on)
import Json.Decode as Json

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model = {t : Time, c : Color}


init : (Model, Cmd Msg)
init =
  (Model 0 darkGreen
    , 
   Cmd.none)



-- UPDATE


type Msg
  = Tick Time | ChangeBackground


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Tick newTime ->
      ({model | t = model.t + 1}, Cmd.none)
    ChangeBackground -> ({model | c = darkRed}, Cmd.none)

onKeyDown message =
    on "keydown"
      (Json.succeed message)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every millisecond Tick



-- VIEW

view model = let t = model.t in
            div [onKeyDown ChangeBackground] 
            [
            collage 500 220 [rect 500 250
                                |> filled darkRed]
            ]

pacman t = group [wedge 100 0.5
                        |> filled yellow
                        |> rotate (Basics.degrees 
                                    (135 - ((abs(cos(t/100))))* 45)
                                    ),
                  wedge 100 0.5
                        |> filled yellow
                        |> rotate (Basics.degrees 
                                    (-135 + ((abs(cos(t/100))))* 45)
                                    )]