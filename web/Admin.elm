import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode exposing (..)
import Json.Decode exposing (..)
import List exposing (..)
import Navigation exposing (..)
import Task exposing (..)
import WebSocket


main =
  Navigation.program SetLocation
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { messages : List String
  , votes : List String
  , passworded : Bool
  , location : Location
  }

type OutgoingMsg = Connecting
                 | NextVote VoteType (List Int)
                 | Reset
                 | KitchenScene
                 | Reel1
                 | Reel2
                 | Reboot
                 | OffsetTime Float
                 | PreshowLoop
                 | ROS1
                 | ROS2
                 | ROS3
                 | Calibration
                 | PaperPlane1
                 | HauntedHouse1
                 | Countdown
                 | PaperPlane2
                 | ROS4
                 | PlaneTracker1
                 | Glitch1
                 | FilmBreak
                 | HackingScene1
                 | HackingScene2
                 | PlaneTracker2
                 | PaperPlane3
                 | ROS5
                 | Memoriam
                 | PaperPlane4
                 | PaperPlane5
                 | Glitch2
                 | FilmBreak2
                 | OperaLoading
                 | Opera
                 | EndingB
                 | EndingC
                 | HauntedHouseEnd
                 | EndCredits
                 | QA
type IncomingMsg = Votes (List String) | PasswordResult Bool
type VoteType = Show | Film

voteType : VoteType -> String
voteType vt =
  case vt of
    Show -> "Show"
    Film -> "Film"

init : Location -> (Model, Cmd Msg)
init loc =
  (Model [] [] False loc, perform Send (Connecting |> encodeOutMsg |> Task.succeed))

encodeOutMsg : OutgoingMsg -> String
encodeOutMsg msg =
  encode 0 <|
    case msg of
      Connecting -> Json.Encode.object [("type", Json.Encode.string "connecting"), ("password", Json.Encode.string "password")]
      NextVote ty is-> Json.Encode.object [("type", Json.Encode.string <| "do" ++ voteType ty ++ "Vote"), ("votes", Json.Encode.list <| List.map Json.Encode.int is)]
      Reset -> Json.Encode.object [("type", Json.Encode.string "reset")]
      KitchenScene -> Json.Encode.object [("type", Json.Encode.string "kitchenScene")]
      Reel1 -> Json.Encode.object [("type", Json.Encode.string "reel1")]
      Reel2 -> Json.Encode.object [("type", Json.Encode.string "reel2")]
      Reboot -> Json.Encode.object [("type", Json.Encode.string "reboot")]
      OffsetTime t -> Json.Encode.object [("type", Json.Encode.string "offsetTime"), ("timeOffset", Json.Encode.float t)]
      PreshowLoop -> Json.Encode.object [("type", Json.Encode.string "preshowloop")]
      ROS1 -> Json.Encode.object [("type", Json.Encode.string "ROS1")]
      ROS2 -> Json.Encode.object [("type", Json.Encode.string "ROS2")]
      ROS3 -> Json.Encode.object [("type", Json.Encode.string "ROS3")]
      Calibration -> Json.Encode.object [("type", Json.Encode.string "calibration")]
      PaperPlane1 -> Json.Encode.object [("type", Json.Encode.string "paperplane1")]
      HauntedHouse1 -> Json.Encode.object [("type", Json.Encode.string "hauntedhou")]
      Countdown -> Json.Encode.object [("type", Json.Encode.string "countdown")]
      PaperPlane2 -> Json.Encode.object [("type", Json.Encode.string "paperplane2")]
      ROS4 -> Json.Encode.object [("type", Json.Encode.string "ROS")]
      PlaneTracker1 -> Json.Encode.object [("type", Json.Encode.string "planetracker1")]
      Glitch1 -> Json.Encode.object [("type", Json.Encode.string "glitch1")]
      FilmBreak -> Json.Encode.object [("type", Json.Encode.string "filmbreak")]
      HackingScene1 -> Json.Encode.object [("type", Json.Encode.string "hackingscene1")]
      HackingScene2 -> Json.Encode.object [("type", Json.Encode.string "hackingscene2")]
      PlaneTracker2 -> Json.Encode.object [("type", Json.Encode.string "planetracker2")]
      PaperPlane3 -> Json.Encode.object [("type", Json.Encode.string "paperplane3")]
      ROS5 -> Json.Encode.object [("type", Json.Encode.string "ROS5")]
      Memoriam -> Json.Encode.object [("type", Json.Encode.string "memoriam")]
      PaperPlane4 -> Json.Encode.object [("type", Json.Encode.string "paperplane4")]
      PaperPlane5 -> Json.Encode.object [("type", Json.Encode.string "paperplane5")]
      Glitch2 -> Json.Encode.object [("type", Json.Encode.string "glitch2")]
      FilmBreak2 -> Json.Encode.object [("type", Json.Encode.string "filmbreak2")]
      OperaLoading -> Json.Encode.object [("type", Json.Encode.string "operaloading")]
      Opera -> Json.Encode.object [("type", Json.Encode.string "opera")]
      EndingB -> Json.Encode.object [("type", Json.Encode.string "endingb")]
      EndingC -> Json.Encode.object [("type", Json.Encode.string "endingc")]
      HauntedHouseEnd -> Json.Encode.object [("type", Json.Encode.string "haunted")]
      EndCredits -> Json.Encode.object [("type", Json.Encode.string "end")]
      QA -> Json.Encode.object [("type", Json.Encode.string "qa")]

decodeInMsg : String -> Result String IncomingMsg
decodeInMsg msg =
  let
    decodeVotes = Json.Decode.map Votes (field "votes" <| Json.Decode.list Json.Decode.string)
    decodePassResult = Json.Decode.map PasswordResult (field "success" <| Json.Decode.bool)
    decodeAll ty =
      case ty of
        "vote" -> decodeVotes
        "password" -> decodePassResult
        _ -> Json.Decode.fail "Message type wrong"
  in
    field "type" Json.Decode.string |> Json.Decode.andThen decodeAll |> flip decodeString msg


-- UPDATE

type Msg
  = Send String
  | NewMessage String
  | SetLocation Location

wsloc : Location -> String
wsloc loc = "ws://" ++ loc.hostname ++ ":9160"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Send wsmsg ->
      (model, WebSocket.send (wsloc model.location) wsmsg)

    SetLocation loc ->
      ({ model | location = loc }, Cmd.none)

    NewMessage str ->
      case decodeInMsg str of
        Ok (Votes vts) ->
          ({ model | votes = vts }, Cmd.none)

        Ok (PasswordResult ps) ->
          ({ model | passworded = ps }, Cmd.none)

        _ ->
          ({ model | messages = str :: model.messages }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen (wsloc model.location) NewMessage


-- VIEW

view : Model -> Html Msg
view model =
  div [] <|
    [ div [] (List.map viewMessage model.messages)
    , div [class "controls"]
      [ div [class "control-group"]
        [ button [onClick (Send <| encodeOutMsg Reset)] [text "Reset"]
        , button [onClick (Send <| encodeOutMsg <| OffsetTime (-10))] [text "RW"]
        , button [onClick (Send <| encodeOutMsg <| OffsetTime 10)] [text "FFW"]
        ]
      , div [class "control-group"]
        [ button [onClick (Send <| encodeOutMsg PreshowLoop)] [text "Preshow"]
        , button [onClick (Send <| encodeOutMsg Calibration)] [text "Calibration"]
        , button [onClick (Send <| encodeOutMsg Reel1)] [text "Reel1"]
        , button [onClick (Send <| encodeOutMsg KitchenScene)] [text "Kitchen Scene"]
        , button [onClick (Send <| encodeOutMsg Reel2)] [text "Reel2"]
        ]
      , div [class "control-group"]
        [ button [onClick (Send <| encodeOutMsg <| NextVote Show [0, 1] )] [text "Show Vote 1"]
        , button [onClick (Send <| encodeOutMsg <| NextVote Show [2, 3, 4] )] [text "Show Vote 2"]
        , button [onClick (Send <| encodeOutMsg <| NextVote Show [5, 6, 7] )] [text "Show Vote 3"]
        , button [onClick (Send <| encodeOutMsg <| NextVote Show [8, 9, 10] )] [text "Show Vote 4"]
        , button [onClick (Send <| encodeOutMsg <| NextVote Show [11, 12, 13] )] [text "Show Vote 5"]
        , button [onClick (Send <| encodeOutMsg <| NextVote Show [14, 15, 16] )] [text "Show Vote 6"]
        ]
      , div [class "control-group"]
        [ button [onClick (Send <| encodeOutMsg <| NextVote Film [1, 2, 3, 4, 6, 7, 8, 9, 11, 12] )] [text "Film Vote 1"]
        , button [onClick (Send <| encodeOutMsg <| NextVote Film [1, 2, 3, 4, 5, 6, 9] )] [text "Film Vote 2"]
        , button [onClick (Send <| encodeOutMsg <| NextVote Film [1, 2, 9, 11] )] [text "Permutations"]
        , button [onClick (Send <| encodeOutMsg <| NextVote Film [13, 14, 15, 16] )] [text "Alt cameras"]
        , button [onClick (Send <| encodeOutMsg <| NextVote Film [17, 18] )] [text "overlays"]
        ]
      , div [class "control-group"]
        [ button [onClick (Send <| encodeOutMsg ROS1)] [text "ROS1"]
        , button [onClick (Send <| encodeOutMsg ROS2)] [text "ROS2"]
        , button [onClick (Send <| encodeOutMsg ROS3)] [text "ROS3"]
        , button [onClick (Send <| encodeOutMsg PaperPlane1)] [text "paperplane1"]
        , button [onClick (Send <| encodeOutMsg HauntedHouse1)] [text "hauntedhou"]
        , button [onClick (Send <| encodeOutMsg Countdown)] [text "countdown"]
        , button [onClick (Send <| encodeOutMsg PaperPlane2)] [text "paperplane2"]
        , button [onClick (Send <| encodeOutMsg ROS4)] [text "ROS"]
        , button [onClick (Send <| encodeOutMsg PlaneTracker1)] [text "planetracker1"]
        , button [onClick (Send <| encodeOutMsg Glitch1)] [text "glitch1"]
        , button [onClick (Send <| encodeOutMsg FilmBreak)] [text "filmbreak"]
        , button [onClick (Send <| encodeOutMsg HackingScene1)] [text "hackingscene1"]
        , button [onClick (Send <| encodeOutMsg HackingScene2)] [text "hackingscene2"]
        , button [onClick (Send <| encodeOutMsg PlaneTracker2)] [text "planetracker2"]
        , button [onClick (Send <| encodeOutMsg PaperPlane3)] [text "paperplane3"]
        , button [onClick (Send <| encodeOutMsg ROS5)] [text "ROS5"]
        , button [onClick (Send <| encodeOutMsg Memoriam)] [text "memoriam"]
        , button [onClick (Send <| encodeOutMsg PaperPlane4)] [text "paperplane4"]
        , button [onClick (Send <| encodeOutMsg PaperPlane5)] [text "paperplane5"]
        , button [onClick (Send <| encodeOutMsg Glitch2)] [text "glitch2"]
        , button [onClick (Send <| encodeOutMsg FilmBreak2)] [text "filmbreak2"]
        , button [onClick (Send <| encodeOutMsg OperaLoading)] [text "opera loading"]
        , button [onClick (Send <| encodeOutMsg Opera)] [text "opera"]
        , button [onClick (Send <| encodeOutMsg EndingB)] [text "endingb"]
        , button [onClick (Send <| encodeOutMsg EndingC)] [text "endingc"]
        , button [onClick (Send <| encodeOutMsg HauntedHouseEnd)] [text "haunted"]
        , button [onClick (Send <| encodeOutMsg EndCredits)] [text "end"]
        , button [onClick (Send <| encodeOutMsg QA)] [text "qa"]
        ]
      ]
    ] ++ indexedMap (\i t -> p [] [text t]) model.votes


viewMessage : String -> Html msg
viewMessage msg =
  div [] [ text msg ]
