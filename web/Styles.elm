module Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (namespace)
import Html.Attributes exposing (style)
import SharedStyles exposing (..)

css =
  (stylesheet << namespace "holme")
  [ button
    [ color (hex "ffffff")
    , fontSize (px 16)
    , backgroundColor (hex "000000")
    , padding4 (px 4) (px 8) (px 4) (px 8)
    , height (px 100)
    , textDecoration none
    , fontFamilies ["Lucida Console", "Monaco", "monospace"]
    , fontSize (Css.em 2)
    , hover
        [ backgroundColor (hex "ffffff")
        , color (hex "000000")
        ]
    , active
        [ backgroundColor (hex "ffffff")
        , color (hex "000000")
        ]
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
