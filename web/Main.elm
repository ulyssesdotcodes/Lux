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
  }

type OutgoingMsg = Connecting | Vote Int | NextVote VoteType (List Int) | Reset | KitchenScene | Underride
type IncomingMsg = Votes (List String)
type VoteType = Show | Film

voteType : VoteType -> String
voteType vt =
  case vt of
    Show -> "Show"
    Film -> "Film"

init : (Model, Cmd Msg)
init =
  (Model [] [], perform Send (Connecting |> encodeOutMsg |> Task.succeed))

encodeOutMsg : OutgoingMsg -> String
encodeOutMsg msg =
  encode 0 <|
    case msg of
      Connecting -> Json.Encode.object [("type", Json.Encode.string "connecting")]
      Vote i -> Json.Encode.object [("type", Json.Encode.string "vote"), ("index", Json.Encode.int i)]
      NextVote ty is-> Json.Encode.object [("type", Json.Encode.string <| "do" ++ voteType ty ++ "Vote"), ("votes", Json.Encode.list <| List.map Json.Encode.int is)]
      Reset -> Json.Encode.object [("type", Json.Encode.string "reset")]
      KitchenScene -> Json.Encode.object [("type", Json.Encode.string "kitchenScene")]
      Underride -> Json.Encode.object [("type", Json.Encode.string "underride")]

decodeInMsg : String -> Result String IncomingMsg
decodeInMsg msg =
  let
    decodeVotes = Json.Decode.map Votes (field "votes" <| Json.Decode.list Json.Decode.string)
  in
    flip decodeString msg <| decodeVotes


-- UPDATE

type Msg
  = Send String
  | NewMessage String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Send wsmsg ->
      (model, WebSocket.send "ws://localhost:9160" wsmsg)

    NewMessage str ->
      case decodeInMsg str of
        Ok (Votes vts) ->
          ({ model | votes = vts }, Cmd.none)

        _ ->
          ({ model | messages = str :: model.messages }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen "ws://localhost:9160" NewMessage


-- VIEW

view : Model -> Html Msg
view model =
  div [] <|
    [ div [] (List.map viewMessage model.messages)
    , button [onClick (Send <| encodeOutMsg Reset)] [text "Reset"]
    , button [onClick (Send <| encodeOutMsg KitchenScene)] [text "Kitchen Scene"]
    , button [onClick (Send <| encodeOutMsg Underride)] [text "Underride"]
    , button [onClick (Send <| encodeOutMsg <| NextVote Show [0, 1, 2] )] [text "Show Vote 1"]
    , button [onClick (Send <| encodeOutMsg <| NextVote Film [0, 1, 2, 3, 4] )] [text "Film Vote 1"]
    , button [onClick (Send <| encodeOutMsg <| NextVote Show [2, 1, 0] )] [text "Show Vote 2"]
    , button [onClick (Send <| encodeOutMsg <| NextVote Film [0, 1, 2, 3, 4] )] [text "Film Vote 2"]
    ] ++ indexedMap (\i t -> button [onClick (Send <| encodeOutMsg <| Vote i)] [text t]) model.votes


viewMessage : String -> Html msg
viewMessage msg =
  div [] [ text msg ]
