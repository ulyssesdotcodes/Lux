module Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (namespace)
import Html.Attributes exposing (style)
import SharedStyles exposing (..)

css =
  (stylesheet << namespace "holme")
  [ button
    [ borderRadius (px 6)
    , color (hex "000000")
    , fontSize (px 16)
    , backgroundColor (hex "ffffff")
    , padding4 (px 4) (px 8) (px 4) (px 8)
    , border3 (px 2) solid (hex "ff7373")
    , textDecoration none
    , hover [ backgroundColor (hex "546ff2")]
    , active [ backgroundColor (hex "546ff2")]
    , disabled [backgroundColor (hex "cccccc") ]
    ]
  , class Control
    [ displayFlex
    , flexDirection row
    ]
  , class ControlGroup
    [ displayFlex
    , flexDirection column
    ]
  , class VotesGroup
    [ displayFlex
    , flexDirection column
    , alignItems center
    , children
      [ button
        [ width (pct 80)
        , marginTop (pct 10)
        ]
      ]
    ]
  ]
