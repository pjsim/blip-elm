module Key exposing (..)


type Key
    = Space
    | ArrowLeft
    | ArrowRight
    | Restart
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        32 ->
            Space

        37 ->
            ArrowLeft

        39 ->
            ArrowRight

        82 ->
            Restart

        _ ->
            Unknown
