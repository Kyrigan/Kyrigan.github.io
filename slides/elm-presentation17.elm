import Graphicsvg exposing (..)
import Html exposing (Html)
import Html.App as Html
import Time exposing (Time, millisecond)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model = Time


init : (Model, Cmd Msg)
init =
  (0, Cmd.none)



-- UPDATE


type Msg
  = Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Tick newTime ->
      (model+1, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every millisecond Tick



-- VIEW

view t = collage 500 500 [pacman t]

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