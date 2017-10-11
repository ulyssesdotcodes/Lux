port module Stylesheets exposing (..)

import Css.File exposing (CssCompilerProgram, CssFileStructure)
import Styles exposing (..)


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "styles.css", Css.File.compile [ Styles.css ] ) ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
