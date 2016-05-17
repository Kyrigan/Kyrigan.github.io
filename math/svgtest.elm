import Html exposing (Html)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, millisecond)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { t : Time
  }

  
init : (Model, Cmd Msg)
init =
  (Model 0, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every millisecond Tick




-- UPDATE


type Msg
    =  Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
      
    Tick newTime ->
        ({model | t = model.t + 1 }, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  let rotation = "rotate(" ++ toString model.t ++ " 50 50)"
  
  in  svg [ viewBox "0 0 100 100", width "300px" ]
          [ rect [height "100", width "100", x "0", y "0", fill "#0B79CE" ] [] ,
            rect [height "30", width "60", x "20", y "35", fill "#BB2222" , transform rotation ] []
        
          ]

