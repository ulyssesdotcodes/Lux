import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode exposing (..)
import Json.Decode exposing (..)
import List exposing (..)
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
  , votes : List (String, Int)
  , location : Location
  , filmVotes : List (Int, String)
  , vcues : List String
  , acues : List String
  }

type OutgoingMsg = Connecting 
                 | DoFilmVote
                 | StartRun
type IncomingMsg = Votes (List (String, Int))
type VoteType = Show | Film

voteType : VoteType -> String
voteType vt =
  case vt of
    Show -> "Show"
    Film -> "Film"

init : Location -> (Model, Cmd Msg)
init loc =
  (Model [] [] loc [] [] [], perform Send (Connecting |> encodeOutMsg |> Task.succeed))

encodeOutMsg : OutgoingMsg -> String
encodeOutMsg msg =
  encode 0 <|
    case msg of
      Connecting -> Json.Encode.object [("type", Json.Encode.string "connecting")
                                       , ("localId", Json.Encode.string "admin")]
      DoFilmVote -> Json.Encode.object [("type", Json.Encode.string "doFilmVote")]
      StartRun -> Json.Encode.object [("type", Json.Encode.string "start")]

decodeInMsg : String -> Result String IncomingMsg
decodeInMsg msg =
  let
    decodeVotes = Json.Decode.map Votes (field "votes" <| Json.Decode.list <| arrayAsTuple2 Json.Decode.string Json.Decode.int)
    decodeAll ty =
      case ty of
        "vote" -> decodeVotes
        _ -> Json.Decode.fail "Message type wrong"
  in
    field "type" Json.Decode.string |> Json.Decode.andThen decodeAll |> flip decodeString msg

arrayAsTuple2 : Decoder a -> Decoder b -> Decoder (a, b)
arrayAsTuple2 a b =
    index 0 a
        |> Json.Decode.andThen (\aVal -> index 1 b
        |> Json.Decode.andThen (\bVal -> Json.Decode.succeed (aVal, bVal)))

-- UPDATE

type Msg
  = Send String
  | NewMessage String
  | SetLocation Location
  | Connect

wsloc : Location -> String
wsloc loc = "ws://" ++ loc.hostname ++ ":9160"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Send wsmsg ->
      (model, WebSocket.send (wsloc model.location) wsmsg)

    SetLocation loc ->
      ({ model | location = loc }, Cmd.none)

    Connect ->
      (model, WebSocket.send (wsloc model.location) <| encodeOutMsg Connecting)

    NewMessage str ->
      case decodeInMsg str of
        Ok (Votes vts) ->
          ({ model | votes = vts }, Cmd.none)

        _ ->
          ({ model | messages = str :: model.messages }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen (wsloc model.location) NewMessage


-- VIEW
button a = Html.button ([style [("font-size", "1em"), ("height", "auto")]] ++ a)

view : Model -> Html Msg
view model =
  div [] <|
    [ div [] (List.map viewMessage model.messages)
    , div [class [Control]]
      [ button [onClick (Send <| encodeOutMsg StartRun)] [text "Start"]
      , button [onClick (Send <| encodeOutMsg DoFilmVote)] [text "Force Vote"]
      ]
    ] ++ indexedMap (\i (t, _) -> p [] [text t]) model.votes

viewMessage : String -> Html msg
viewMessage msg =
  div [] [ text msg ]
