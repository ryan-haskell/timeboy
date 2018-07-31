module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder, dict, keyValuePairs, string, int, field)
import Dict exposing (Dict)


-- Main


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
    , timeInMinutes : Int
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


type alias TimeJson =
    { timeEntries : List ( String, TimeEntryJson )
    , users : List ( String, UserJson )
    , stories : Dict String StoryJson
    , workspaces : Dict String WorkspaceJson
    }


type alias TimeEntryJson =
    { datePerformed : String
    , timeInMinutes : Int
    , notes : String
    , storyId : String
    , workspaceId : String
    }


type alias UserJson =
    { fullName : String
    , photoUrl : String
    , headline : String
    }


type alias StoryJson =
    { title : String
    }


type alias WorkspaceJson =
    { title : String
    , description : String
    }


timeJsonDecoder : Decoder TimeJson
timeJsonDecoder =
    Decode.map4 TimeJson
        (field "time_entries" <| keyValuePairs timeEntryJsonDecoder)
        (field "users" <| keyValuePairs userJsonDecoder)
        (field "stories" <| dict storyJsonDecoder)
        (field "workspaces" <| dict workspaceJsonDecoder)


timeEntryJsonDecoder : Decoder TimeEntryJson
timeEntryJsonDecoder =
    Decode.map5 TimeEntryJson
        (field "date_performed" string)
        (field "time_in_minutes" int)
        (field "notes" string)
        (field "story_id" string)
        (field "workspace_id" string)


userJsonDecoder : Decoder UserJson
userJsonDecoder =
    Decode.map3 UserJson
        (field "full_name" string)
        (field "photo_path" string)
        (field "headline" string)


storyJsonDecoder : Decoder StoryJson
storyJsonDecoder =
    Decode.map StoryJson
        (field "title" string)


workspaceJsonDecoder : Decoder WorkspaceJson
workspaceJsonDecoder =
    Decode.map2 WorkspaceJson
        (field "title" string)
        (field "description" string)


timeDataDecoder : Decoder TimeData
timeDataDecoder =
    timeJsonDecoder
        |> Decode.map (toTimeData)


toTimeData : TimeJson -> TimeData
toTimeData json =
    TimeData
        (usernameFromJson json)
        (daysFromJson json)


usernameFromJson : TimeJson -> String
usernameFromJson { users } =
    users
        |> List.map (\( _, user ) -> user.fullName)
        |> List.head
        |> Maybe.withDefault "You"


daysFromJson : TimeJson -> List Day
daysFromJson json =
    json.timeEntries
        |> List.map (\( _, entryJson ) -> entryJson)
        |> List.map (\entry -> ( entry.datePerformed, (toTimeEntry json entry) ))
        |> List.foldl makeDictOfEntries Dict.empty
        |> Dict.toList
        |> List.map (\( date, entries ) -> Day date entries)


toTimeEntry : TimeJson -> TimeEntryJson -> Entry
toTimeEntry json entryJson =
    Entry
        (Dict.get entryJson.workspaceId json.workspaces
            |> Maybe.map (\workspace -> workspace.title)
            |> Maybe.withDefault entryJson.workspaceId
        )
        (Dict.get entryJson.storyId json.stories
            |> Maybe.map (\story -> story.title)
            |> Maybe.withDefault entryJson.storyId
        )
        (entryJson.timeInMinutes)
        (entryJson.notes)


makeDictOfEntries : ( String, Entry ) -> Dict String (List Entry) -> Dict String (List Entry)
makeDictOfEntries ( date, entry ) dict =
    let
        entries : List Entry
        entries =
            Dict.get date dict
                |> Maybe.withDefault []
                |> (\entries -> entries ++ [ entry ])
    in
        Dict.insert date entries dict


parseJson : String -> Result String TimeData
parseJson =
    Decode.decodeString timeDataDecoder



-- View


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ case model.state of
            NothingYet ->
                viewMainMenu Nothing model

            Success data ->
                viewTimeEntries data

            Failure reason ->
                viewMainMenu (Just reason) model
        ]


mavenlinkApiUrl : String
mavenlinkApiUrl =
    "https://api.mavenlink.com/api/v1/time_entries.json?date_performed_between=2018-07-01:&per_page=200&order=date_performed&include=user,story,workspace"


viewMainMenu : Maybe String -> Model -> Html Msg
viewMainMenu error model =
    div [ class "main-menu" ]
        [ h1 [ class "main-menu__title" ] [ text "Timeboy" ]
        , h3 [ class "main-menu__subtitle" ]
            [ text "brought to you by "
            , a [ href "http://elm-lang.org", target "_blank" ] [ text "elm" ]
            , text "."
            ]
        , ol [ class "main-menu__instructions" ]
            (List.map (\htmls -> li [ class "main-menu__instructions" ] htmls) <|
                [ [ text "Login to "
                  , a [ href "https://onenorth.mavenlink.com", target "_blank" ] [ text "Mavenlink" ]
                  , text "."
                  ]
                , [ text "Copy all the JSON from "
                  , a [ href mavenlinkApiUrl, target "_blank" ] [ text "here." ]
                  ]
                , [ text "Paste it below, and click 'Go'!"
                  ]
                ]
            )
        , label [ class "textarea" ]
            [ p [ class "textarea__label" ] [ text "JSON" ]
            , textarea
                [ class "textarea__input"
                , value model.textarea
                , onInput UpdateTextArea
                ]
                []
            ]
        , button [ onClick (SetTimeData (parseJson model.textarea)) ] [ text "Go!" ]
        , case error of
            Just reason ->
                let
                    _ =
                        Debug.log "Error" reason
                in
                    p [ class "error" ]
                        [ text "Please try to copy that JSON again..." ]

            Nothing ->
                text ""
        ]


viewTimeEntries : TimeData -> Html Msg
viewTimeEntries { username, days } =
    div [ class "time-entries" ]
        [ h1 [ class "time-entries__title" ] [ text username ]
        , h2 [ class "time-entries__subtitle" ] [ text <| toString (List.length days) ++ " days to move over." ]
        , div [ class "days" ] (List.map viewDay days)
        ]


viewDay : Day -> Html Msg
viewDay { date, entries } =
    div [ class "day" ]
        [ h3 [ class "day__title" ] [ text date ]
        , div [ class "entries" ] (List.map viewEntry entries)
        ]


viewEntry : Entry -> Html Msg
viewEntry entry =
    div [ class "entry" ]
        [ h4 [ class "entry__title" ] [ text entry.project ]
        , h5 [ class "entry__subtitle" ] [ text entry.subproject ]
        , p [ class "entry__time" ]
            [ entry.timeInMinutes
                |> toHours
                |> toString
                |> (\hours -> hours ++ " hours.")
                |> text
            ]
        , p [ class "entry__notes" ] [ text entry.notes ]
        ]


toHours : Int -> Float
toHours minutes =
    toFloat (minutes // 60) + (toFloat (rem minutes 60)) / 60.0
