module SharedStyles exposing (..)

import Html.CssHelpers exposing (withNamespace)

type CssClasses
    = Control
    | ControlGroup
    | VotesGroup



hNamespace =
    withNamespace "holme"
