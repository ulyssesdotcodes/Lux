port module Main exposing (..)

import Char
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
import Random exposing (..)

import SharedStyles exposing (..)

{ id, class, classList } = hNamespace


main : Program (Maybe SavedModel) Model Msg
main =
  Navigation.programWithFlags SetLocation
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Saving id

port setStorage : SavedModel -> Cmd msg

saveGeneratedId : String -> Msg
saveGeneratedId newId = SaveId newId

generateId : Generator String
generateId = Random.map String.fromList <| Random.list 10 <| Random.map (Char.fromCode) (Random.int 65 91)

-- MODEL

type alias Model =
  { messages : List String
  , votes : List (String, Int)
  , password : String
  , connected : Bool
  , location : Location
  , votedNum : Maybe Int
  , savedModel : Maybe SavedModel
  }

type alias SavedModel = { localId : String }


type OutgoingMsg = Connecting String | Vote String Int
type IncomingMsg = Votes (List (String, Int)) (Maybe Int)
type VoteType = Show | Film

voteType : VoteType -> String
voteType vt =
  case vt of
    Show -> "Show"
    Film -> "Film"

init :  Maybe SavedModel -> Location -> (Model, Cmd Msg)
init saved loc =
  (Model [] [] "" False loc Nothing saved, perform (always Connect) (Task.succeed ()))

-- perform Send (Connecting |> encodeOutMsg |> Task.succeed)

encodeOutMsg : OutgoingMsg -> String
encodeOutMsg msg =
  encode 0 <|
    case msg of
      Connecting localId -> Json.Encode.object [("type", Json.Encode.string "connecting"), ("localId", Json.Encode.string localId)]
      Vote localId i -> Json.Encode.object [("type", Json.Encode.string "vote"), ("index", Json.Encode.int i), ("localId", Json.Encode.string localId)]

decodeInMsg : String -> Result String IncomingMsg
decodeInMsg msg =
  let
    decodeVotes = Json.Decode.map2 Votes (field "votes" <| Json.Decode.list <| arrayAsTuple2 Json.Decode.string Json.Decode.int) (field "votedNum" <| Json.Decode.nullable Json.Decode.int)
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
  = Connect
  | NewMessage String
  | SetLocation Location
  | ChooseVote Int
  | SaveId String

wsloc : Location -> String
wsloc loc = "ws://" ++ loc.hostname ++ ":9160"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChooseVote i ->
      case model.savedModel of
        Just savedModel -> 
          ({ model | votedNum = Just i }, if model.votedNum == Nothing then WebSocket.send (wsloc model.location) <| encodeOutMsg (Vote savedModel.localId i) else Cmd.none)
        Nothing ->
          (model, generate saveGeneratedId generateId)
      

    Connect ->
      case model.savedModel of
        Just savedModel -> 
          (model, WebSocket.send (wsloc model.location) <| encodeOutMsg (Connecting savedModel.localId))
        Nothing ->
          (model, generate saveGeneratedId generateId)

    NewMessage str ->
      case decodeInMsg str of
        Ok (Votes vts vtd) ->
          case model.votes == vts of
            True -> (model, Cmd.none)
            False ->
              ({ model | votes = vts, votedNum = vtd}, Cmd.none)

        _ ->
          ({ model | messages = str :: model.messages }, Cmd.none)

    SetLocation loc ->
      ({ model | location = loc }, Cmd.none)

    SaveId newId ->
      let
        newModel = SavedModel newId
      in
        ({ model | savedModel = Just newModel }, Cmd.batch [setStorage newModel, perform (always Connect) (Task.succeed ())])




-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen (wsloc model.location) NewMessage

-- VIEW

view : Model -> Html Msg
view model =
  div [class [VotesGroup]] <| [
    p [] [text "LUX wants ", 
    u [] [text "you"], 
    text " to have the most ", 
    sup [] [text "FUN"]]] ++
    (indexedMap (\i (t, voteCount) -> 
      div [] [button ([onClick <| ChooseVote i] ++ [disabled <| withDefault False <| Maybe.map ((/=) i) model.votedNum]) [text t], text (toString voteCount)]) model.votes)

viewMessage : String -> Html msg
viewMessage msg =
  div [] [ text msg ]
