import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode exposing (..)
import Json.Decode exposing (..)
import WebSocket
import List exposing (..)
import Task exposing (..)


main =
  Html.program
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
  }

type OutgoingMsg = Connecting String | Vote Int
type IncomingMsg = Votes (List String) | PasswordResult Bool
type VoteType = Show | Film

voteType : VoteType -> String
voteType vt =
  case vt of
    Show -> "Show"
    Film -> "Film"

init : (Model, Cmd Msg)
init =
  (Model [] [] "" False, Cmd.none)

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
  = Send String
  | NewMessage String
  | Password String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Send wsmsg ->
      (model, WebSocket.send "ws://localhost:9160" wsmsg)

    NewMessage str ->
      case decodeInMsg str of
        Ok (Votes vts) ->
          ({ model | votes = vts }, Cmd.none)

        Ok (PasswordResult True) ->
          ({ model | connected = True }, Cmd.none)

        _ ->
          ({ model | messages = str :: model.messages }, Cmd.none)

    Password ps ->
      ({ model | password = ps }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen "ws://localhost:9160" NewMessage

-- VIEW

view : Model -> Html Msg
view model =
  div [] <|
    case model.connected of
      True -> indexedMap (\i t -> button [onClick (Send <| encodeOutMsg <| Vote i)] [text t]) model.votes
      False ->
        [input [onInput Password] [], button [onClick (Send <| encodeOutMsg <| Connecting model.password)] [text "connect"]]


viewMessage : String -> Html msg
viewMessage msg =
  div [] [ text msg ]
