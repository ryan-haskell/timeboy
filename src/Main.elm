module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- Model


type alias Model =
    { textarea : String
    , state : ModelState
    }


type ModelState
    = NothingYet
    | Success TimeData
    | Failure String


type alias TimeData =
    { username : String
    , days : List Day
    }


type alias Day =
    { date : String
    , entries : List Entry
    }


type alias Entry =
    { project : String
    , subproject : String
    , minutes : Float
    , notes : String
    }


model : Model
model =
    Model
        ""
        NothingYet



-- Update


type Msg
    = UpdateTextArea String
    | SetTimeData (Result String TimeData)


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTextArea content ->
            { model | textarea = content }

        SetTimeData (Ok data) ->
            { model | state = Success data }

        SetTimeData (Err reason) ->
            { model | state = Failure reason }



timeData : Decoder TimeData
timeData =

-- View


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ case model.state of
            NothingYet ->
                viewMainMenu model

            Success data ->
                viewTimeEntries data

            Failure reason ->
                viewErrorMessage reason
        ]


viewMainMenu : Model -> Html Msg
viewMainMenu model =
    div [ class "main-menu" ]
        [ h1 [] [ text "Timeboy" ]
        , h3 []
            [ text "brought to you by "
            , a [ href "http://elm-lang.org", target "_blank" ] [ text "elm" ]
            , text "."
            ]
        , textarea [] []
        , button [ onClick (SetTimeData (parseJson model)) ]
        ]


viewTimeEntries : TimeData -> Html Msg
viewTimeEntries data =
    div [] [ text "Success!" ]


viewErrorMessage : String -> Html Msg
viewErrorMessage reason =
    div [] [ text ("Error " ++ reason) ]
