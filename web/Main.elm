import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode exposing (..)
import Json.Decode exposing (..)
import List exposing (..)
import Maybe exposing (..)
import Navigation exposing (..)
import Task exposing (..)
import WebSocket

import SharedStyles exposing (..)

{ id, class, classList } = hNamespace


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
  , password : String
  , connected : Bool
  , location : Location
  , votedNum : Maybe Int
  }

type OutgoingMsg = Connecting String | Vote Int
type IncomingMsg = Votes (List String) | PasswordResult Bool
type VoteType = Show | Film

voteType : VoteType -> String
voteType vt =
  case vt of
    Show -> "Show"
    Film -> "Film"

init : Location -> (Model, Cmd Msg)
init loc =
  (Model [] [] "" False loc Nothing, perform Connect ("password" |> Task.succeed))

-- perform Send (Connecting |> encodeOutMsg |> Task.succeed)

encodeOutMsg : OutgoingMsg -> String
encodeOutMsg msg =
  encode 0 <|
    case msg of
      Connecting ps -> Json.Encode.object [("type", Json.Encode.string "connecting"), ("password", Json.Encode.string ps)]
      Vote i -> Json.Encode.object [("type", Json.Encode.string "vote"), ("index", Json.Encode.int i)]

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
  = Connect String
  | NewMessage String
  | Password String
  | SetLocation Location
  | ChooseVote Int

wsloc : Location -> String
wsloc loc = "ws://" ++ loc.hostname ++ ":9160"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChooseVote i ->
      ({ model | votedNum = Just i }, if model.votedNum == Nothing then WebSocket.send (wsloc model.location) <| encodeOutMsg (Vote i) else Cmd.none)

    Connect pass ->
      (model, WebSocket.send (wsloc model.location) <| encodeOutMsg (Connecting pass))


    NewMessage str ->
      case decodeInMsg str of
        Ok (Votes vts) ->
          case model.votes == vts of
            True -> (model, Cmd.none)
            False ->
              ({ model | votes = vts, votedNum = Nothing}, Cmd.none)

        Ok (PasswordResult True) ->
          ({ model | connected = True }, Cmd.none)

        _ ->
          ({ model | messages = str :: model.messages }, Cmd.none)

    Password ps ->
      ({ model | password = ps }, Cmd.none)

    SetLocation loc ->
      ({ model | location = loc }, Cmd.none)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen (wsloc model.location) NewMessage

-- VIEW

view : Model -> Html Msg
view model =
  div [class [VotesGroup]] <| [p [] [text "LUX wants ", u [] [text "you"], text " to have the most ", sup [] [text "FUN"]]] ++
    case model.connected of
      True -> indexedMap (\i t -> button ([onClick <| ChooseVote i] ++ [disabled <| withDefault False <| Maybe.map ((/=) i) model.votedNum]) [text t]) model.votes
      False ->
        [input [onInput Password] [], button [onClick <| Connect model.password] [text "connect"]]


viewMessage : String -> Html msg
viewMessage msg =
  div [] [ text msg ]
